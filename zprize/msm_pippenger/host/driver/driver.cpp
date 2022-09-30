#include "xcl2.hpp"

#include "bls12_377_g1/bls12_377_g1.h"
#include "bls12_377_g1/pippenger.h"
#include "bls12_377_g1/rust_types.h"
#include "bls12_377_g1/log_time.h"

#include <experimental/xrt_device.h>
#include <experimental/xrt_kernel.h>

#include <fstream>
#include <vector>

#define BITS_PER_INPUT_POINT 377 * 3
#define BITS_PER_OUTPUT_POINT 377 * 4
#define SCALAR_BITS 253
#define DDR_BITS 512

// We round up our points to the nearest multiple of the AXI stream / DDR
// #define BYTES_PER_INPUT \
//   (((SCALAR_BITS + BITS_PER_INPUT_POINT + DDR_BITS - 1) / DDR_BITS) * DDR_BITS) / 8
#define BYTES_PER_INPUT_POINT \
  ((((BITS_PER_INPUT_POINT + DDR_BITS - 1) / DDR_BITS) * DDR_BITS) / 8)
#define BYTES_PER_OUTPUT (((BITS_PER_OUTPUT_POINT + DDR_BITS - 1) / DDR_BITS) * DDR_BITS) / 8
#define BYTES_PER_INPUT_SCALAR 32

#define LOG_MAX_NUM_POINTS_PER_CHUNK 23
#define MAX_NUM_INPUTS_PER_CHUNK (1ull << LOG_MAX_NUM_POINTS_PER_CHUNK)

#define UINT32_PER_INPUT_POINT (BYTES_PER_INPUT_POINT / 4)
#define UINT32_PER_INPUT_SCALAR (BYTES_PER_INPUT_SCALAR / 4)

#define NUM_OUTPUT_POINTS 90091
#define OUTPUT_SIZE_IN_BYTES (BYTES_PER_OUTPUT * NUM_OUTPUT_POINTS)
#define OUTPUT_SIZE_IN_UINT32 (OUTPUT_SIZE_IN_BYTES / 4)

typedef std::vector<uint32_t, aligned_allocator<uint32_t> > aligned_vec32;

static uint32_t
round_up_to_multiple_of_16(uint32_t x) {
  return (x + 15) >> 4 << 4;
}

struct PostProcessingValues {
  bls12_377_g1::Xyzt accum;
  bls12_377_g1::Xyzt running;
  bls12_377_g1::Xyzt bucket_sum;
  bls12_377_g1::Xyzt final_result;
  bls12_377_g1::GeneralUnifiedAddIntoTemps temps;
};

// Driver class - maintains the set of points
class Driver {
private:
  aligned_vec32 source_kernel_input_points;
  aligned_vec32 source_kernel_input_scalars;
  aligned_vec32 source_kernel_output;

  // OpenCL stuff
  cl::CommandQueue q;
  cl::Context context;
  cl::Kernel krnl_mm2s_points;
  cl::Kernel krnl_mm2s_scalars;
  cl::Kernel krnl_msm_pippenger;
  cl::Kernel krnl_s2mm;
  std::vector<cl::Buffer> buffer_input_points;
  std::vector<cl::Buffer> buffer_input_scalars;
  cl::Buffer buffer_output;

  // preallocated objects to be used for post-processing
  PostProcessingValues post_processing_values;

public:
  const uint64_t total_num_points;

  Driver(g1_affine_t *rust_points, ssize_t npoints)
    : total_num_points(npoints),
      source_kernel_input_points(npoints * UINT32_PER_INPUT_POINT),
      source_kernel_input_scalars(npoints * UINT32_PER_INPUT_SCALAR),
      source_kernel_output(OUTPUT_SIZE_IN_UINT32)
  {
    // memset(source_kernel_input_points.data(), 0, sizeof(uint32_t) * source_kernel_input_points.size());
    // memset(source_kernel_input_scalars.data(), 0, sizeof(uint32_t) * source_kernel_input_scalars.size());
    // memset(source_kernel_output.data(), 0, sizeof(uint32_t) * source_kernel_output.size());

    std::cout << "Converting affine points into internal format ..." << std::endl;
    bls12_377_g1::Xyzt point;
    uint32_t *ptr_point = source_kernel_input_points.data();

    for (ssize_t i = 0; i < npoints; i++) {
      // std::cout << rust_points[i] << std::endl;
      point.copy_from_rust_type(rust_points[i]);
      point.preComputeFPGA();
      point.copy_to_fpga_buffer(ptr_point);
      // point.println();

      ptr_point += UINT32_PER_INPUT_POINT;

      if ((i + 1) % (1 << 20) == 0) {
        std::cout << "Converted " << (i + 1) << " points ..." << std::endl;
      }
    }
    std::cout << "Done internal format conversion!" << std::endl;
  }

  inline uint64_t num_input_chunks() {
    return (total_num_points + MAX_NUM_INPUTS_PER_CHUNK - 1) >> LOG_MAX_NUM_POINTS_PER_CHUNK;
  }

  inline uint64_t num_points_in_last_chunk() {
    return total_num_points - ((num_input_chunks()  - 1) << LOG_MAX_NUM_POINTS_PER_CHUNK);
  }

  void load_xclbin(const std::string& binaryFile) {
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
      OCL_CHECK(err, context = cl::Context(device, nullptr, nullptr, nullptr, &err));
      OCL_CHECK(err, q = cl::CommandQueue(
                         context, device,
                         CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE | CL_QUEUE_PROFILING_ENABLE, &err));

      std::cout << "Trying to program device[" << i << "]: " << device.getInfo<CL_DEVICE_NAME>()
                << std::endl;
      cl::Program program(context, {device}, bins, nullptr, &err);
      if (err != CL_SUCCESS) {
        std::cout << "Failed to program device[" << i << "] with xclbin file!\n";
      } else {
        std::cout << "Device[" << i << "]: program successful!\n";
        OCL_CHECK(err, krnl_mm2s_points = cl::Kernel(program, "krnl_mm2s", &err));
        OCL_CHECK(err, krnl_mm2s_scalars = cl::Kernel(program, "krnl_mm2s", &err));
        OCL_CHECK(err, krnl_msm_pippenger = cl::Kernel(program, "krnl_msm_pippenger", &err));
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
      uint64_t num_points_in_chunk = MAX_NUM_INPUTS_PER_CHUNK;
      if (chunk_id == num_input_chunks() - 1) {
        num_points_in_chunk = num_points_in_last_chunk();
      }

      OCL_CHECK(
          err,
          buffer_input_points.emplace_back(context,
            CL_MEM_USE_HOST_PTR | CL_MEM_READ_ONLY | CL_MEM_HOST_WRITE_ONLY,
            num_points_in_chunk * BYTES_PER_INPUT_POINT,
            source_kernel_input_points.data() + (chunk_id * MAX_NUM_INPUTS_PER_CHUNK * UINT32_PER_INPUT_POINT),
            &err));
      OCL_CHECK(
          err,
          buffer_input_scalars.emplace_back(context,
            CL_MEM_USE_HOST_PTR | CL_MEM_READ_ONLY | CL_MEM_HOST_WRITE_ONLY,
            num_points_in_chunk * BYTES_PER_INPUT_SCALAR,
            source_kernel_input_scalars.data() + (chunk_id * MAX_NUM_INPUTS_PER_CHUNK * UINT32_PER_INPUT_SCALAR),
            &err));

    }
    OCL_CHECK(err,  buffer_output = cl::Buffer(context,
                                               CL_MEM_USE_HOST_PTR | CL_MEM_WRITE_ONLY | CL_MEM_HOST_READ_ONLY,
                                               OUTPUT_SIZE_IN_BYTES,
                                               source_kernel_output.data(),
                                               &err));

    // Load points into the FPGA 
    {
      bench("Copying input points to gmem", [&]() {
          cl_int err;

          // Trick openCL to know which DDR to stream buffer_inputs to
          for (auto buffer_input : buffer_input_points) {
            OCL_CHECK(err, err = krnl_mm2s_points.setArg(0, buffer_input));
            OCL_CHECK(err, err = q.enqueueMigrateMemObjects({buffer_input}, 0 /* 0 means from host*/, nullptr));
          }
          OCL_CHECK(err, err = q.finish());
      });
    }
  }

  void postProcess(const uint32_t *source_kernel_output) {
    const uint64_t NUM_32B_WORDS_PER_OUTPUT = BYTES_PER_OUTPUT / 4;

    bls12_377_g1::init();

    post_processing_values.final_result.setToIdentity();

    uint64_t bit_offset = 0;
    uint64_t point_idx = 0;
    for (uint64_t window_idx = 0; window_idx < bls12_377_g1::NUM_WINDOWS; window_idx++) {
      const auto CUR_WINDOW_LEN = bls12_377_g1::NUM_WINDOW_BITS(window_idx);
      const auto CUR_NUM_BUCKETS = bls12_377_g1::NUM_BUCKETS(window_idx);

      // perform triangle sum
      post_processing_values.accum.setToIdentity();
      post_processing_values.running.setToIdentity();
      for (uint64_t bucket_idx = CUR_NUM_BUCKETS - 1; bucket_idx >= 1 /* skip bucket 0 */;
           bucket_idx--) {
        // receive fpga point
        post_processing_values.bucket_sum.import_from_fpga_vector(
            source_kernel_output + (NUM_32B_WORDS_PER_OUTPUT * point_idx));
        post_processing_values.bucket_sum.postComputeFPGA();
        ++point_idx;

        // do triangle sum update
        bls12_377_g1::triangleSumUpdate(
            post_processing_values.accum,
            post_processing_values.running,
            post_processing_values.bucket_sum,
            post_processing_values.temps
            );
      }
      bls12_377_g1::finalSumUpdate(
          post_processing_values.final_result,
          post_processing_values.accum,
          bit_offset,
          post_processing_values.temps
          );
      bit_offset += CUR_WINDOW_LEN;
    }
    if (point_idx != NUM_OUTPUT_POINTS) {
      printf("ERROR IN point_idx\n");
    }

    // final_result.println();
    post_processing_values.final_result.extendedTwistedEdwardsToWeierstrass();
  }

  inline void feed_msm(g1_projective_t *out, biginteger256_t *scalars) {
    bench("memcpy-ing scalars to special memory region", [&]() {
        // uint32_t *ptr_scalar = source_kernel_input_scalars.data();

        // for (uint64_t i = 0; i < total_num_points; i++) {
        //   // load the scalar
        //   scalars[i].copy_to_fpga_buffer(ptr_scalar);

        //   ptr_scalar += UINT32_PER_INPUT_SCALAR;
        // }

        // We known source_kernel_inputs.scalrs.data() is 8192 aligned, because
        // we set it up as such. But we no thing about scalars, since that's
        // directly from rust.
        void *dst = __builtin_assume_aligned(
            (void*) source_kernel_input_scalars.data(), 8192);
        memcpy(
            dst,
            (void*) scalars,
            UINT32_PER_INPUT_SCALAR * sizeof(uint32_t) * total_num_points
        );
    });

    bench("transferring scalars to gmem", [&]() {
        cl_int err;

        // Trick openCL to know which DDR to stream buffer_inputs to
        for (auto buffer_input : buffer_input_scalars) {
          OCL_CHECK(err, err = krnl_mm2s_scalars.setArg(0, buffer_input));
          OCL_CHECK(err, err = q.enqueueMigrateMemObjects({buffer_input}, 0 /* 0 means from host*/, nullptr));
        }
        OCL_CHECK(err, err = q.finish());
    });

    bench("Doing FPGA Computation", [&]() {
        cl_int err;

        // Set the writer kernel arguments and dispatch them
        for (uint64_t chunk_id = 0; chunk_id < num_input_chunks(); chunk_id++) {
          uint64_t num_points_in_chunk = MAX_NUM_INPUTS_PER_CHUNK;
          bool is_last_chunk = chunk_id == num_input_chunks() - 1;
          if (is_last_chunk) {
            num_points_in_chunk = num_points_in_last_chunk();
          }

          OCL_CHECK(err, err = krnl_mm2s_points.setArg(0, buffer_input_points[chunk_id]));
          OCL_CHECK(err, err = krnl_mm2s_points.setArg(2, round_up_to_multiple_of_16(
                                                              num_points_in_chunk * UINT32_PER_INPUT_POINT)));
          OCL_CHECK(err, err = krnl_mm2s_points.setArg(3, is_last_chunk));
          OCL_CHECK(err, err = q.enqueueTask(krnl_mm2s_points));

          OCL_CHECK(err, err = krnl_mm2s_scalars.setArg(0, buffer_input_scalars[chunk_id]));
          OCL_CHECK(err, err = krnl_mm2s_scalars.setArg(2, round_up_to_multiple_of_16(
                                                              num_points_in_chunk * UINT32_PER_INPUT_SCALAR)));
          OCL_CHECK(err, err = krnl_mm2s_scalars.setArg(3, is_last_chunk));
          OCL_CHECK(err, err = q.enqueueTask(krnl_mm2s_scalars));
        }
        std::cout << "Launched writer kernels!" << std::endl;

        // Launch the reader kernel arguments and dispatch them
        OCL_CHECK(err, err = krnl_s2mm.setArg(0, buffer_output));
        OCL_CHECK(err, err = krnl_s2mm.setArg(2, uint32_t(OUTPUT_SIZE_IN_UINT32)));
        OCL_CHECK(err, err = q.enqueueTask(krnl_s2mm));
        std::cout << "Launched reader kernel!" << std::endl;

        // Wait for kernels to finish its operation
        OCL_CHECK(err, err = q.finish());
    });

    bench("Copying results back from gmem", [&]() {
        cl_int err;

        // Copy Result from Device Global Memory to Host Local Memory
        OCL_CHECK(err, err = q.enqueueMigrateMemObjects({buffer_output}, CL_MIGRATE_MEM_OBJECT_HOST));
        OCL_CHECK(err, err = q.finish());
    });

    bench("Doing on-host postprocessing", [&]() {
        postProcess(source_kernel_output.data());
        post_processing_values.final_result.copy_to_rust_type(*out);
    });
  }
};

extern "C" Driver *msm_init(const char *xclbin, ssize_t xclbin_len, g1_affine_t *rust_points,
                            ssize_t npoints) {
  bls12_377_g1::init();

  std::cout << "Instantiating msm driver for " << npoints << " points" << std::endl;
  auto *driver = new Driver(rust_points, npoints);

  std::string binaryFile(xclbin, xclbin_len);
  std::cout << "Loading XCLBIN=" << binaryFile << " and doing openCL setups:" << std::endl;
  driver->load_xclbin(binaryFile);

  return driver;
}

extern "C" void msm_mult(Driver *driver, g1_projective_t *out, uint64_t batch_size,
                         biginteger256_t *scalars) {
  for (uint64_t i = 0; i < batch_size; i++) {
    printf("Running MSM (Batch %lu) with [%lu] input points\n", i, driver->total_num_points);
    printf("Number of input chunks = %lu\n", driver->num_input_chunks());
    printf("Number of points in last chunk = 0x%016x\n", driver->num_points_in_last_chunk());

    driver->feed_msm(out + i, scalars + (i * driver->total_num_points));
  }
}
