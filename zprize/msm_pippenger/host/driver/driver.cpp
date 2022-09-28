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
#define BYTES_PER_INPUT \
  (((SCALAR_BITS + BITS_PER_INPUT_POINT + DDR_BITS - 1) / DDR_BITS) * DDR_BITS) / 8
#define BYTES_PER_OUTPUT (((BITS_PER_OUTPUT_POINT + DDR_BITS - 1) / DDR_BITS) * DDR_BITS) / 8

#define LOG_MAX_NUM_POINTS_PER_CHUNK 23
#define MAX_NUM_INPUTS_PER_CHUNK (1ull << LOG_MAX_NUM_POINTS_PER_CHUNK)

#define UINT32_PER_INPUT (BYTES_PER_INPUT / 4)

// Driver class - maintains the set of points
class Driver {
 private:
  std::vector<bls12_377_g1::Xyzt> points;
  const std::string binaryFile;

 public:
  Driver(const std::vector<bls12_377_g1::Xyzt> &points, const std::string &binaryFile)
      : points(points), binaryFile(binaryFile) {}

  const uint64_t NUM_OUTPUT_POINTS = 90091;
  const uint64_t OUTPUT_SIZE_IN_UINT32 = (BYTES_PER_OUTPUT * NUM_OUTPUT_POINTS) / 4;

  inline uint64_t total_num_points() { return points.size(); }

  inline uint64_t num_input_chunks() {
    return (total_num_points() + MAX_NUM_INPUTS_PER_CHUNK - 1) >> LOG_MAX_NUM_POINTS_PER_CHUNK;
  }

  inline uint64_t num_points_in_last_chunk() {
    return total_num_points() - ((num_input_chunks()  - 1) << LOG_MAX_NUM_POINTS_PER_CHUNK);
  }

  // inline uint64_t total_input_size_in_uint32_per_msm() { return (BYTES_PER_INPUT * numPoints()) / 4; }

  bls12_377_g1::Xyzt postProcess(const uint32_t *source_kernel_output) {
    const uint64_t NUM_32B_WORDS_PER_OUTPUT = BYTES_PER_OUTPUT / 4;

    bls12_377_g1::init();
    bls12_377_g1::Xyzt final_result;
    final_result.setToIdentity();

    bls12_377_g1::Xyzt accum, running;
    uint64_t bit_offset = 0;
    uint64_t point_idx = 0;
    for (uint64_t window_idx = 0; window_idx < bls12_377_g1::NUM_WINDOWS; window_idx++) {
      const auto CUR_WINDOW_LEN = bls12_377_g1::NUM_WINDOW_BITS(window_idx);
      const auto CUR_NUM_BUCKETS = bls12_377_g1::NUM_BUCKETS(window_idx);

      // perform triangle sum
      bls12_377_g1::Xyzt bucket_sums[CUR_NUM_BUCKETS];
      accum.setToIdentity();
      running.setToIdentity();
      for (uint64_t bucket_idx = CUR_NUM_BUCKETS - 1; bucket_idx >= 1 /* skip bucket 0 */;
           bucket_idx--) {
        auto &bucket_sum = bucket_sums[bucket_idx];

        // receive fpga point
        bucket_sum.import_from_fpga_vector(source_kernel_output +
                                           (NUM_32B_WORDS_PER_OUTPUT * point_idx));
        bucket_sum.postComputeFPGA();
        ++point_idx;

        // do triangle sum update
        bls12_377_g1::triangleSumUpdate(accum, running, bucket_sum);
      }
      bls12_377_g1::finalSumUpdate(final_result, accum, bit_offset);
      bit_offset += CUR_WINDOW_LEN;
    }
    if (point_idx != NUM_OUTPUT_POINTS) {
      printf("ERROR IN point_idx\n");
    }

    // final_result.println();
    final_result.extendedTwistedEdwardsToWeierstrass();
    return final_result;
  }

  inline void feed_msm(g1_projective_t *out, biginteger256_t *scalars) {
    // Allocate Memory in Host Memory
    uint64_t vector_output_size_bytes = sizeof(int) * OUTPUT_SIZE_IN_UINT32;

    std::vector<uint32_t, aligned_allocator<uint32_t> > source_kernel_input(total_num_points() * UINT32_PER_INPUT);
    std::vector<uint32_t, aligned_allocator<uint32_t> > source_kernel_output(OUTPUT_SIZE_IN_UINT32);
    memset(source_kernel_input.data(), 0, sizeof(uint32_t) * source_kernel_input.size());
    memset(source_kernel_output.data(), 0, sizeof(uint32_t) * source_kernel_output.size());

    // Load points from library representation
    {
      uint32_t *base_ptr = source_kernel_input.data();
      printf("Bytes per input = 0x%016x", BYTES_PER_INPUT);
      for (uint64_t i = 0; i < total_num_points(); i++) {
        const uint64_t NUM_32B_WORDS_PER_SCALAR = 256 / 32;
        const uint64_t NUM_32B_WORDS_PER_POINT = (256 * 5) / 32;
        // uint32_t *base_ptr = &source_kernel_input[i * (BYTES_PER_INPUT / 4)];

        // load the point
        points[i].copy_to_fpga_buffer(base_ptr);
        // load the scalar
        scalars[i].copy_to_fpga_buffer(base_ptr + (1152 / 32));
        base_ptr += (BYTES_PER_INPUT) / 4;
      }
    }

    cl_int err;
    cl::CommandQueue q;
    cl::Context context;
    cl::Kernel krnl_mm2s, krnl_msm_pippenger, krnl_s2mm;
    // OPENCL HOST CODE AREA START
    // Create Program and Kernel
    auto devices = xcl::get_xil_devices();
    auto device = devices[0];

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
        OCL_CHECK(err, krnl_mm2s = cl::Kernel(program, "krnl_mm2s", &err));
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

    // Allocate Buffer in Global Memory
    std::vector<cl::Buffer> buffer_inputs;
    for (uint64_t chunk_id = 0; chunk_id < num_input_chunks(); chunk_id++) {
      uint64_t num_points_in_chunk = MAX_NUM_INPUTS_PER_CHUNK;
      if (chunk_id == num_input_chunks() - 1) {
        num_points_in_chunk = num_points_in_last_chunk();
      }
      uint64_t num_bytes_in_chunk = num_points_in_chunk * BYTES_PER_INPUT;

      OCL_CHECK(err, buffer_inputs.emplace_back(context,
          	                                    CL_MEM_USE_HOST_PTR | CL_MEM_READ_ONLY,
          	                                    num_bytes_in_chunk,
                                                source_kernel_input.data() + (chunk_id * MAX_NUM_INPUTS_PER_CHUNK * UINT32_PER_INPUT),
                                                &err));
    }
    OCL_CHECK(err, cl::Buffer buffer_output(context,
                                            CL_MEM_USE_HOST_PTR | CL_MEM_WRITE_ONLY,
                                            vector_output_size_bytes,
                                            source_kernel_output.data(),
                                            &err));

    // Copy input data to device global memory
    bench("Copying scalars and points to gmem", [&]() {
        // Trick openCL to know which DDR to stream buffer_inputs to
        for (auto buffer_input : buffer_inputs) {
          OCL_CHECK(err, err = krnl_mm2s.setArg(0, buffer_input));
          OCL_CHECK(err, err = q.enqueueMigrateMemObjects({buffer_input}, 0 /* 0 means from host*/, nullptr));
        }
        OCL_CHECK(err, err = q.finish());
    });

    bench("Doing actual work", [&]() {
        // Set the writer kernel arguments and dispatch them
        for (uint64_t chunk_id = 0; chunk_id < num_input_chunks(); chunk_id++) {
          uint64_t num_points_in_chunk = MAX_NUM_INPUTS_PER_CHUNK;
          bool is_last_chunk = chunk_id == num_input_chunks() - 1;
          if (is_last_chunk) {
            num_points_in_chunk = num_points_in_last_chunk();
          }
          uint32_t num_uint32_in_chunk = num_points_in_chunk * UINT32_PER_INPUT;

          OCL_CHECK(err, err = krnl_mm2s.setArg(0, buffer_inputs[chunk_id]));
          OCL_CHECK(err, err = krnl_mm2s.setArg(2, num_uint32_in_chunk));
          OCL_CHECK(err, err = krnl_mm2s.setArg(3, is_last_chunk));
          OCL_CHECK(err, err = q.enqueueTask(krnl_mm2s));
        }
        std::cout << "Launched writer kernel!" << std::endl;

        // Launch the reader kernel arguments and dispatch them
        OCL_CHECK(err, err = krnl_s2mm.setArg(0, buffer_output));
        OCL_CHECK(err, err = krnl_s2mm.setArg(2, uint32_t(OUTPUT_SIZE_IN_UINT32)));
        OCL_CHECK(err, err = q.enqueueTask(krnl_s2mm));
        std::cout << "Launched reader kernel!" << std::endl;

        // Wait for kernels to finish its operation
        OCL_CHECK(err, err = q.finish());
    });

    bench("Copying results back from gmem", [&]() {
      // Copy Result from Device Global Memory to Host Local Memory
      OCL_CHECK(err, err = q.enqueueMigrateMemObjects({buffer_output}, CL_MIGRATE_MEM_OBJECT_HOST));
      OCL_CHECK(err, err = q.finish());
    });

    // OPENCL HOST CODE AREA END
    auto final_result = postProcess(source_kernel_output.data());
    final_result.copy_to_rust_type(*out);
  }
};

extern "C" Driver *msm_init(const char *xclbin, ssize_t xclbin_len, g1_affine_t *rust_points,
                            ssize_t npoints) {
  std::string binaryFile(xclbin, xclbin_len);
  printf("Initializing with XCLBIN=%s\n", binaryFile.c_str());
  bls12_377_g1::init();
  std::vector<bls12_377_g1::Xyzt> points(npoints);
  for (ssize_t i = 0; i < npoints; i++) {
    // std::cout << rust_points[i] << std::endl;
    points[i].copy_from_rust_type(rust_points[i]);
    points[i].preComputeFPGA();
    // points[i].println();
  }
  auto *driver = new Driver(points, binaryFile);
  return driver;
}

extern "C" void msm_mult(Driver *driver, g1_projective_t *out, uint64_t batch_size,
                         biginteger256_t *scalars) {
  for (uint64_t i = 0; i < batch_size; i++) {
    printf("Running MSM (Batch %lu) with [%lu] input points\n", i, driver->total_num_points());
    printf("Number of input chunks = %lu\n", driver->num_input_chunks());
    printf("Number of points in last chunk = 0x%016x\n", driver->num_points_in_last_chunk());

    driver->feed_msm(out + i, scalars + (i * driver->total_num_points()));
  }
}
