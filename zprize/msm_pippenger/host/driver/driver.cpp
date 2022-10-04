#include <experimental/xrt_device.h>
#include <experimental/xrt_kernel.h>

#include <fstream>
#include <vector>

#include "bls12_377_g1/bls12_377_g1.h"
#include "bls12_377_g1/log_time.h"
#include "bls12_377_g1/pippenger.h"
#include "bls12_377_g1/rust_types.h"
#include "xcl2.hpp"

#define BITS_PER_INPUT_POINT 377 * 3
#define BITS_PER_OUTPUT_POINT 377 * 4
#define SCALAR_BITS 253
#define DDR_BITS 512

// We round up our points to the nearest multiple of the AXI stream / DDR
// #define BYTES_PER_INPUT \
//   (((SCALAR_BITS + BITS_PER_INPUT_POINT + DDR_BITS - 1) / DDR_BITS) *
//   DDR_BITS) / 8
#define BYTES_PER_INPUT_POINT \
  ((((BITS_PER_INPUT_POINT + DDR_BITS - 1) / DDR_BITS) * DDR_BITS) / 8)
#define BYTES_PER_OUTPUT \
  (((BITS_PER_OUTPUT_POINT + DDR_BITS - 1) / DDR_BITS) * DDR_BITS) / 8
#define BYTES_PER_INPUT_SCALAR 32

#define UINT32_PER_INPUT_POINT (BYTES_PER_INPUT_POINT / 4)
#define UINT32_PER_INPUT_SCALAR (BYTES_PER_INPUT_SCALAR / 4)

#define NUM_OUTPUT_POINTS 69631
#define OUTPUT_SIZE_IN_BYTES (BYTES_PER_OUTPUT * NUM_OUTPUT_POINTS)
#define OUTPUT_SIZE_IN_UINT32 (OUTPUT_SIZE_IN_BYTES / 4)

/* Setting [mask_io] to false will give a detailed breakdown, but it will have
 * worse performance because we block for IO. It will however display useful
 * runtime breakdowns.
 */
static const bool mask_io = true;

/* Setting [debug] will add some print statements. They don't affect runtime
 * much, but might be noise to the user.
 */
static const bool debug = false;

typedef std::vector<uint32_t, aligned_allocator<uint32_t> > aligned_vec32;

static uint32_t round_up_to_multiple_of_16(uint32_t x) {
  return (x + 15) >> 4 << 4;
}

static uint32_t calc_log_max_num_points_per_chunk(uint64_t npoints) {
  /* [max_allowed_num_chunks] should be chosen such that
     [TIME_MSM / max_allowed_num_chunks] is strictly greater than
     [time_post_processing + time_memcpy_one_chunk + overhead] to ensure that we
     always keep the FPGA busy.

     We also don't want too many chunks! As it will use up a lot of memory.
   */
  const uint64_t max_allowed_num_chunks = 4;

  /* [minimum_log_max_num_points_per_chunk] is chosen such that
     [source_kernel_input_points.data() +  (x <<
     minimum_log_max_num_points_per_chunk)] and
     [source_kernel_input_scalars.data() +  (x <<
     minimum_log_max_num_points_per_chunk)] will be a 8192-byte aligned address
     forall x.

     Every scalar 256bits = 32bytes => 32 * 1024 % 8192 == 0
  */
  const uint64_t minimum_log_max_num_points_per_chunk = 10;

  uint64_t log_max_num_points_per_chunk = minimum_log_max_num_points_per_chunk;

  while (max_allowed_num_chunks * (1ull << log_max_num_points_per_chunk) <
         npoints) {
    log_max_num_points_per_chunk++;
  }
  return log_max_num_points_per_chunk;
}

struct PostProcessingValues {
  bls12_377_g1::Xyzt accum;
  bls12_377_g1::Xyzt running;
  bls12_377_g1::Xyzt bucket_sum;
  bls12_377_g1::Xyzt final_result;
  bls12_377_g1::GeneralUnifiedAddIntoTemps temps;
};

struct Events {
  std::vector<cl::Event> ev_transfer_scalars_to_fpga;
  std::vector<cl::Event> ev_krnl_mm2s_scalars;
  std::vector<cl::Event> ev_krnl_mm2s_points;
  cl::Event ev_krnl_s2mm_output_current;
  cl::Event ev_krnl_s2mm_output_previous;
  cl::Event ev_transfer_output_to_host_a;
  cl::Event ev_transfer_output_to_host_b;
};

// Driver class - maintains the set of points
class Driver {
 private:
  const uint64_t log_max_num_points_per_chunk;

  aligned_vec32 source_kernel_input_points;
  aligned_vec32 source_kernel_input_scalars;
  aligned_vec32 source_kernel_output_a;
  aligned_vec32 source_kernel_output_b;

  // host compute for non-convertible points
  std::vector<uint64_t> non_convertible_indices;
  std::vector<bls12_377_g1::Xyzt> non_convertible_points;

  // OpenCL stuff
  cl::CommandQueue q;
  cl::Context context;
  cl::Kernel krnl_mm2s_points;
  cl::Kernel krnl_mm2s_scalars;
  cl::Kernel krnl_msm_pippenger;
  cl::Kernel krnl_s2mm;
  std::vector<cl::Buffer> buffer_input_points;
  std::vector<cl::Buffer> buffer_input_scalars;
  Events events;
  cl::Buffer buffer_output_a;
  cl::Buffer buffer_output_b;

  // preallocated objects to be used for post-processing
  PostProcessingValues post_processing_values;

 public:
  const uint64_t total_num_points;

  inline uint64_t max_num_points_per_chunk() {
    return 1ull << log_max_num_points_per_chunk;
  }

  Driver(g1_affine_t *rust_points, ssize_t npoints)
      : total_num_points(npoints),
        source_kernel_input_points(npoints * UINT32_PER_INPUT_POINT),
        source_kernel_input_scalars(npoints * UINT32_PER_INPUT_SCALAR),
        source_kernel_output_a(OUTPUT_SIZE_IN_UINT32),
        source_kernel_output_b(OUTPUT_SIZE_IN_UINT32),
        log_max_num_points_per_chunk(
            calc_log_max_num_points_per_chunk(npoints)) {
    // memset(source_kernel_input_points.data(), 0, sizeof(uint32_t) *
    // source_kernel_input_points.size());
    // memset(source_kernel_input_scalars.data(), 0, sizeof(uint32_t) *
    // source_kernel_input_scalars.size()); memset(source_kernel_output.data(),
    // 0, sizeof(uint32_t) * source_kernel_output.size());

    std::cout << "Converting affine points into internal format (This takes "
                 "awhile...) ..."
              << std::endl;
    bls12_377_g1::Xyzt point;
    uint32_t *ptr_point = source_kernel_input_points.data();
    uint64_t unconvertible_points = 0;

    for (ssize_t i = 0; i < npoints; i++) {
      // std::cout << rust_points[i] << std::endl;
      bool convertible = point.copy_from_rust_type(rust_points[i]);
      if (convertible) {
        point.preComputeFPGA(post_processing_values.temps);
        point.copy_to_fpga_buffer(ptr_point);
        // point.println();

      } else {
        // add point to host compute buffer
        non_convertible_points.push_back(point);
        non_convertible_indices.push_back((uint64_t)i);
        unconvertible_points++;
      }
      ptr_point += UINT32_PER_INPUT_POINT;

      // Print every 1M points so the user doesn't think we're deadlocked
      if ((i + 1) % (1 << 20) == 0) {
        std::cout << "Converted " << (i + 1) << " points ..." << std::endl;
      }
    }

    if (unconvertible_points) {
      std::cout
          << "Found " << unconvertible_points
          << " unconvertible points! These points will be handled in the host"
          << std::endl;
    }
    std::cout << "Done internal format conversion!" << std::endl;
  }

  uint64_t get_num_points_in_chunk(uint64_t chunk_index) {
    if (chunk_index == num_input_chunks() - 1) {
      return num_points_in_last_chunk();
    }

    return max_num_points_per_chunk();
  }

  inline uint64_t num_input_chunks() {
    return (total_num_points + max_num_points_per_chunk() - 1) >>
           log_max_num_points_per_chunk;
  }

  inline uint64_t num_points_in_last_chunk() {
    return total_num_points -
           ((num_input_chunks() - 1) << log_max_num_points_per_chunk);
  }

  void load_xclbin(const std::string &binaryFile) {
    // Create Program and Kernel
    auto devices = xcl::get_xil_devices();
    auto device = devices[0];
    cl_int err;

    // read_binary_file() is a utility API which will load the binaryFile
    // and will return the pointer to file buffer.
    auto fileBuf = xcl::read_binary_file(binaryFile);
    cl::Program::Binaries bins{{fileBuf.data(), fileBuf.size()}};
    bool valid_device = false;
    for (uint64_t i = 0; i < devices.size(); i++) {
      auto device = devices[i];
      // Creating Context and Command Queue for selected Device
      OCL_CHECK(err,
                context = cl::Context(device, nullptr, nullptr, nullptr, &err));
      OCL_CHECK(err,
                q = cl::CommandQueue(context, device,
                                     CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE |
                                         CL_QUEUE_PROFILING_ENABLE,
                                     &err));

      std::cout << "Trying to program device[" << i
                << "]: " << device.getInfo<CL_DEVICE_NAME>() << std::endl;
      cl::Program program(context, {device}, bins, nullptr, &err);
      if (err != CL_SUCCESS) {
        std::cout << "Failed to program device[" << i
                  << "] with xclbin file!\n";
      } else {
        std::cout << "Device[" << i << "]: program successful!\n";
        OCL_CHECK(err,
                  krnl_mm2s_points = cl::Kernel(program, "krnl_mm2s", &err));
        OCL_CHECK(err,
                  krnl_mm2s_scalars = cl::Kernel(program, "krnl_mm2s", &err));
        OCL_CHECK(err, krnl_msm_pippenger =
                           cl::Kernel(program, "krnl_msm_pippenger", &err));
        OCL_CHECK(err, krnl_s2mm = cl::Kernel(program, "krnl_s2mm", &err));
        valid_device = true;
        break;  // we break because we found a valid device
      }
    }
    if (!valid_device) {
      std::cout << "Failed to program any device found, exit!\n";
      exit(EXIT_FAILURE);
    }

    // Allocate openCL Buffers
    for (uint64_t chunk_id = 0; chunk_id < num_input_chunks(); chunk_id++) {
      uint64_t num_points_in_chunk = max_num_points_per_chunk();
      if (chunk_id == num_input_chunks() - 1) {
        num_points_in_chunk = num_points_in_last_chunk();
      }

      OCL_CHECK(err, buffer_input_points.emplace_back(
                         context, CL_MEM_USE_HOST_PTR | CL_MEM_READ_ONLY,
                         num_points_in_chunk * BYTES_PER_INPUT_POINT,
                         source_kernel_input_points.data() +
                             (chunk_id * max_num_points_per_chunk() *
                              UINT32_PER_INPUT_POINT),
                         &err));
      OCL_CHECK(err, buffer_input_scalars.emplace_back(
                         context, CL_MEM_USE_HOST_PTR | CL_MEM_READ_ONLY,
                         num_points_in_chunk * BYTES_PER_INPUT_SCALAR,
                         source_kernel_input_scalars.data() +
                             (chunk_id * max_num_points_per_chunk() *
                              UINT32_PER_INPUT_SCALAR),
                         &err));
    }
    OCL_CHECK(err,
              buffer_output_a = cl::Buffer(
                  context, CL_MEM_USE_HOST_PTR | CL_MEM_WRITE_ONLY,
                  OUTPUT_SIZE_IN_BYTES, source_kernel_output_a.data(), &err));
    OCL_CHECK(err,
              buffer_output_b = cl::Buffer(
                  context, CL_MEM_USE_HOST_PTR | CL_MEM_WRITE_ONLY,
                  OUTPUT_SIZE_IN_BYTES, source_kernel_output_b.data(), &err));

    // Allocate event objects
    events.ev_transfer_scalars_to_fpga.resize(num_input_chunks());
    events.ev_krnl_mm2s_scalars.resize(num_input_chunks());
    events.ev_krnl_mm2s_points.resize(num_input_chunks());

    // Load points into the FPGA
    {
      bench("Copying input points to gmem", [&]() {
        cl_int err;

        // Trick openCL to know which DDR to stream buffer_inputs to
        for (auto buffer_input : buffer_input_points) {
          OCL_CHECK(err, err = krnl_mm2s_points.setArg(0, buffer_input));
          OCL_CHECK(err,
                    err = q.enqueueMigrateMemObjects(
                        {buffer_input}, 0 /* 0 means from host*/, nullptr));
        }
        OCL_CHECK(err, err = q.finish());
      });
    }
  }

  void postProcess(const uint32_t *source_kernel_output, int batch_num, const biginteger256_t *scalars) {
    const uint64_t NUM_32B_WORDS_PER_OUTPUT = BYTES_PER_OUTPUT / 4;

    post_processing_values.final_result.setToTwistedEdwardsIdentity();

    uint64_t bit_offset = 0;
    uint64_t point_idx = 0;
    for (uint64_t window_idx = 0; window_idx < bls12_377_g1::NUM_WINDOWS;
         window_idx++) {
      const auto CUR_WINDOW_LEN = bls12_377_g1::NUM_WINDOW_BITS(window_idx);
      const auto CUR_NUM_BUCKETS = bls12_377_g1::NUM_BUCKETS(window_idx);

      // perform triangle sum
      post_processing_values.accum.setToTwistedEdwardsIdentity();
      post_processing_values.running.setToTwistedEdwardsIdentity();

      // need signed int because of >= check
      for (int64_t bucket_idx = CUR_NUM_BUCKETS - 1; bucket_idx >= 0;
           bucket_idx--) {
        // receive fpga point
        post_processing_values.bucket_sum.import_from_fpga_vector(
            source_kernel_output + (NUM_32B_WORDS_PER_OUTPUT * point_idx));
        post_processing_values.bucket_sum.postComputeFPGA(
            post_processing_values.temps);
        ++point_idx;

        // do triangle sum update
        bls12_377_g1::triangleSumUpdate(
            post_processing_values.accum, post_processing_values.running,
            post_processing_values.bucket_sum, post_processing_values.temps);
      }
      bls12_377_g1::finalSumUpdate(post_processing_values.final_result,
                                   post_processing_values.accum, bit_offset,
                                   post_processing_values.temps);
      bit_offset += CUR_WINDOW_LEN;
    }
    // std::cout << point_idx << std::endl;
    if (point_idx != NUM_OUTPUT_POINTS) {
      printf("ERROR IN point_idx\n");
    }

    // final_result.println();

    // In the overwhelming case, we don't need to do the final weierstrass patching
    if (__builtin_expect(non_convertible_points.empty(), 1)) {
      post_processing_values.final_result.extendedTwistedEdwardsToWeierstrassInMontgomerySpace();
      return;
    }

    // add all the weierstrass points
    // printf(" *** RAHUL: adding in non convertible points (batch %d)***\n",
    //       batch_num);

    post_processing_values.final_result.extendedTwistedEdwardsToWeierstrass();

    // printf(" *** RAHUL: points.size() = %lu, indices.size() = %lu***\n", non_convertible_points.size(), non_convertible_indices.size(), batch_num);
    assert(non_convertible_points.size() == non_convertible_indices.size());
    for (size_t i = 0; i < non_convertible_points.size(); i++) {
      uint64_t idx = non_convertible_indices[i];
      // printf(" *** RAHUL: point (%lu), index = %lu", i, idx);
      auto *scalar_ptr = (scalars + (batch_num * total_num_points) + idx);
      // std::cout << *scalar_ptr << std::endl;

      weierstrassMultiplyAndAdd(post_processing_values.final_result, non_convertible_points[i],
                                                                    *scalar_ptr);
    }
    post_processing_values.final_result.weistrassValuesInMontgomerySpace();
    // printf("finished postProcess\n");
    // fflush(stdout);
  }

  inline uint32_t *get_input_scalars_pointer() {
    // We known source_kernel_inputs.scalrs.data() is 8192 aligned, because
    // we set it up as such. But we know nothing about scalars, since that's
    // directly from rust.
    return (uint32_t *)__builtin_assume_aligned(
        (void *)source_kernel_input_scalars.data(), 8192);
  }

  void memcpy_in_scalars_chunk(biginteger256_t *scalars, uint64_t scalars_start,
                               uint64_t scalars_size, uint64_t buffer_start,
                               int batch_num) {
    uint32_t *ptr_device_input_scalar =
        get_input_scalars_pointer() + (buffer_start * UINT32_PER_INPUT_SCALAR);
    uint64_t scalars_end = scalars_start + scalars_size;

    // std::cout << "FIRST SCALAR IN CHUNK: " << *(scalars + scalars_start) << std::endl;
    // copy in all the points
    memcpy(ptr_device_input_scalar, (void *)(scalars + scalars_start),
           UINT32_PER_INPUT_SCALAR * sizeof(uint32_t) * scalars_size);

    // remove the non-convertible points and save them to buffer
    for (const auto &idx : non_convertible_indices) {
      uint64_t scalars_start_idx = scalars_start - (batch_num * total_num_points);
      uint64_t scalars_end_idx = scalars_end - (batch_num * total_num_points);
      // printf("non convertible idx: %lu; start, end = %lu, %lu\n", idx, scalars_start_idx, scalars_end_idx);
      if ((scalars_start_idx <= idx) && (idx < scalars_end_idx)) {
        memset(ptr_device_input_scalar + UINT32_PER_INPUT_SCALAR * (idx - scalars_start_idx), 0,
               UINT32_PER_INPUT_SCALAR * sizeof(uint32_t));
      }
    }
  }

  inline void run_single_batch(g1_projective_t *out, biginteger256_t *scalars,
                               int batch_num) {
    bench("memcpy-ing scalars to special memory region", [&]() {
      // uint32_t *ptr_scalar = source_kernel_input_scalars.data();

      // for (uint64_t i = 0; i < total_num_points; i++) {
      //   // load the scalar
      //   scalars[i].copy_to_fpga_buffer(ptr_scalar);

      //   ptr_scalar += UINT32_PER_INPUT_SCALAR;
      // }
      memcpy_in_scalars_chunk(scalars, 0, total_num_points, 0, batch_num);
    });

    bench("transferring scalars to gmem", [&]() {
      cl_int err;

      // Trick openCL to know which DDR to stream buffer_inputs to
      for (auto buffer_input : buffer_input_scalars) {
        OCL_CHECK(err, err = krnl_mm2s_scalars.setArg(0, buffer_input));
        OCL_CHECK(err, err = q.enqueueMigrateMemObjects(
                           {buffer_input}, 0 /* 0 means from host*/, nullptr));
      }
      OCL_CHECK(err, err = q.finish());
    });

    bench("Doing FPGA Computation", [&]() {
      cl_int err;

      // Set the writer kernel arguments and dispatch them
      for (uint64_t chunk_id = 0; chunk_id < num_input_chunks(); chunk_id++) {
        uint64_t num_points_in_chunk = max_num_points_per_chunk();
        bool is_last_chunk = chunk_id == num_input_chunks() - 1;
        if (is_last_chunk) {
          num_points_in_chunk = num_points_in_last_chunk();
        }

        OCL_CHECK(err, err = krnl_mm2s_points.setArg(
                           0, buffer_input_points[chunk_id]));
        OCL_CHECK(err,
                  err = krnl_mm2s_points.setArg(
                      2, round_up_to_multiple_of_16(num_points_in_chunk *
                                                    UINT32_PER_INPUT_POINT)));
        OCL_CHECK(err, err = krnl_mm2s_points.setArg(3, is_last_chunk));
        OCL_CHECK(err, err = q.enqueueTask(krnl_mm2s_points));

        OCL_CHECK(err, err = krnl_mm2s_scalars.setArg(
                           0, buffer_input_scalars[chunk_id]));
        OCL_CHECK(err,
                  err = krnl_mm2s_scalars.setArg(
                      2, round_up_to_multiple_of_16(num_points_in_chunk *
                                                    UINT32_PER_INPUT_SCALAR)));
        OCL_CHECK(err, err = krnl_mm2s_scalars.setArg(3, is_last_chunk));
        OCL_CHECK(err, err = q.enqueueTask(krnl_mm2s_scalars));
      }
      // std::cout << "Launched writer kernels!" << std::endl;

      // Launch the reader kernel arguments and dispatch them
      OCL_CHECK(err, err = krnl_s2mm.setArg(0, buffer_output_a));
      OCL_CHECK(err,
                err = krnl_s2mm.setArg(2, uint32_t(OUTPUT_SIZE_IN_UINT32)));
      OCL_CHECK(err, err = q.enqueueTask(krnl_s2mm));
      // std::cout << "Launched reader kernel!" << std::endl;

      // Wait for kernels to finish its operation
      OCL_CHECK(err, err = q.finish());
    });

    bench("Copying results back from gmem", [&]() {
      cl_int err;

      // Copy Result from Device Global Memory to Host Local Memory
      OCL_CHECK(err, err = q.enqueueMigrateMemObjects(
                         {buffer_output_a}, CL_MIGRATE_MEM_OBJECT_HOST));
      OCL_CHECK(err, err = q.finish());
    });

    bench("Doing on-host postprocessing", [&]() {
      postProcess(source_kernel_output_a.data(), batch_num, scalars);
      post_processing_values.final_result.copy_to_rust_type(*out);
    });
  }

  /* I can't get the event-based API to work, so this is a terrible workaround
   * that relies on q.finish() to decide what events to be dispatched.
   */
  inline void run_asynchronous(g1_projective_t *out, biginteger256_t *scalars,
                               uint64_t num_batches) {
    bool has_output_to_transfer = false;
    bool has_output_to_process = false;
    uint64_t transferred_outputs = 0;
    uint64_t processed_outputs = 0;
    cl_int err;

    auto do_postprocessing = [&]() {
      // printf("doing postProcess(batch = %d)\n", processed_outputs);
      postProcess((processed_outputs % 2 == 0 ? source_kernel_output_a.data()
                                              : source_kernel_output_b.data()),
                  processed_outputs, scalars);
      post_processing_values.final_result.copy_to_rust_type(*out);
      processed_outputs++;
      out++;
    };

    auto do_work_for_output = [&]() {
      if (has_output_to_transfer) {
        // Copy Result from Device Global Memory to Host Local Memory
        OCL_CHECK(err, err = q.enqueueMigrateMemObjects(
                           {(transferred_outputs % 2 == 0 ? buffer_output_a
                                                          : buffer_output_b)},
                           CL_MIGRATE_MEM_OBJECT_HOST));
        transferred_outputs++;
      }

      /* We used to call q.flush() here to dispatch all work to the FPGA. But
         experimentally, it made the XRT runtime will behave like q.finish(),
         which will block until the queue is empty..

         Fortunately, when we enqueue a task, it basically gets to the FPGA
         ~immediately. So we don't need to flush anything.
       */

      if (has_output_to_process) {
        if (debug) {
          bench("Doing on-host postprocessing", do_postprocessing);
        } else {
          do_postprocessing();
        }
      }

      has_output_to_process = has_output_to_transfer;
    };

    uint64_t scalars_start = 0;
    for (uint64_t b = 0; b < num_batches; b++) {
      /* Enqueue fpga->host transfer for this batch. */
      for (uint64_t chunk_index = 0; chunk_index < num_input_chunks();
           chunk_index++) {
        uint64_t num_points_in_chunk = get_num_points_in_chunk(chunk_index);
        bool is_last_chunk = chunk_index == num_input_chunks() - 1;

        memcpy_in_scalars_chunk(scalars, scalars_start, num_points_in_chunk,
                                chunk_index * max_num_points_per_chunk(), b);
        OCL_CHECK(err, err = q.enqueueMigrateMemObjects(
                           {buffer_input_scalars[chunk_index]},
                           0 /* 0 means from host*/, nullptr));

        OCL_CHECK(err, err = q.finish());

        OCL_CHECK(err, err = krnl_mm2s_points.setArg(
                           0, buffer_input_points[chunk_index]));
        OCL_CHECK(err,
                  err = krnl_mm2s_points.setArg(
                      2, round_up_to_multiple_of_16(num_points_in_chunk *
                                                    UINT32_PER_INPUT_POINT)));
        OCL_CHECK(err, err = krnl_mm2s_points.setArg(3, is_last_chunk));
        OCL_CHECK(err, err = q.enqueueTask(krnl_mm2s_points));

        OCL_CHECK(err, err = krnl_mm2s_scalars.setArg(
                           0, buffer_input_scalars[chunk_index]));
        OCL_CHECK(err,
                  err = krnl_mm2s_scalars.setArg(
                      2, round_up_to_multiple_of_16(num_points_in_chunk *
                                                    UINT32_PER_INPUT_SCALAR)));
        OCL_CHECK(err, err = krnl_mm2s_scalars.setArg(3, is_last_chunk));
        OCL_CHECK(err, err = q.enqueueTask(krnl_mm2s_scalars));

        if (is_last_chunk) {
          OCL_CHECK(err,
                    err = krnl_s2mm.setArg(
                        0, (b % 2 == 0 ? buffer_output_a : buffer_output_b)));
          OCL_CHECK(err,
                    err = krnl_s2mm.setArg(2, uint32_t(OUTPUT_SIZE_IN_UINT32)));
          OCL_CHECK(err, err = q.enqueueTask(krnl_s2mm));
        }

        do_work_for_output();

        has_output_to_transfer = is_last_chunk;

        scalars_start += num_points_in_chunk;
      }
    }

    while (processed_outputs < num_batches) {
      OCL_CHECK(err, err = q.finish());

      do_work_for_output();

      has_output_to_transfer = false;
    }
  }
};

extern "C" Driver *msm_init(const char *xclbin, ssize_t xclbin_len,
                            g1_affine_t *rust_points, ssize_t npoints) {
  bls12_377_g1::init();

  std::cout << "\n\nInstantiating msm driver for " << npoints << " points"
            << std::endl;
  auto *driver = new Driver(rust_points, npoints);

  std::string binaryFile(xclbin, xclbin_len);
  std::cout << "Loading XCLBIN=" << binaryFile
            << " and doing openCL setups:" << std::endl;
  driver->load_xclbin(binaryFile);

  return driver;
}

extern "C" void msm_mult(Driver *driver, g1_projective_t *out,
                         uint64_t num_batches, biginteger256_t *ptr_scalars) {
  printf("Running MSM of [%lu] input points (%lu batches)\n",
         driver->total_num_points, num_batches);
  printf("Streaming input scalars across %lu chunks per batch (%s)\n",
         driver->num_input_chunks(),
         mask_io ? "Mask IO and Post Processing" : "Synchronous");

  if (mask_io) {
    driver->run_asynchronous(out, ptr_scalars, num_batches);

  } else {
    for (uint64_t i = 0; i < num_batches; i++) {
      driver->run_single_batch(out + i,
                               ptr_scalars + (i * driver->total_num_points), i);
    }
  }
}
