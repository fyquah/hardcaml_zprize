#include "xcl2.hpp"

#include "bls12_377_g1/bls12_377_g1.h"
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

// Driver class - maintains the set of points
class Driver {
 private:
  std::vector<bls12_377_g1::Xyzt> points;
  const std::string binaryFile;

 public:
  Driver(const std::vector<bls12_377_g1::Xyzt> &points, const std::string &binaryFile)
      : points(points), binaryFile(binaryFile) {}

  const int NUM_OUTPUT_POINTS = 90091;
  const size_t OUTPUT_SIZE = (BYTES_PER_OUTPUT * NUM_OUTPUT_POINTS) / 4;

  inline uint64_t numPoints() { return points.size(); }
  inline auto INPUT_SIZE() { return (BYTES_PER_INPUT * numPoints()) / 4; }

  bls12_377_g1::Xyzt postProcess(const uint32_t *source_kernel_output) {
    const size_t NUM_32B_WORDS_PER_OUTPUT = BYTES_PER_OUTPUT / 4;

    bls12_377_g1::init();
    bls12_377_g1::Xyzt final_result;
    final_result.setToIdentity();

    bls12_377_g1::Xyzt accum, running;
    int bit_offset = 0;
    int point_idx = 0;
    for (int window_idx = 0; window_idx < bls12_377_g1::NUM_WINDOWS; window_idx++) {
      const auto CUR_WINDOW_LEN = bls12_377_g1::NUM_WINDOW_BITS(window_idx);
      const auto CUR_NUM_BUCKETS = bls12_377_g1::NUM_BUCKETS(window_idx);

      // perform triangle sum
      bls12_377_g1::Xyzt bucket_sums[CUR_NUM_BUCKETS];
      accum.setToIdentity();
      running.setToIdentity();
      for (int bucket_idx = CUR_NUM_BUCKETS - 1; bucket_idx >= 1 /* skip bucket 0 */;
           bucket_idx--) {
        auto &bucket_sum = bucket_sums[bucket_idx];

        // receive fpga point
        bucket_sum.import_from_fpga_vector(source_kernel_output +
                                           (NUM_32B_WORDS_PER_OUTPUT * point_idx));
        ++point_idx;

        // do triangle sum update
        bls12_377_g1::triangleSumUpdate(accum, running, bucket_sum);
      }
      bls12_377_g1::finalSumUpdate(final_result, accum, bit_offset);
      bit_offset += CUR_WINDOW_LEN;
    }

    final_result.extendedTwistedEdwardsToWeierstrass();
    return final_result;
  }

  inline void feed_msm(g1_projective_t *out, biginteger256_t *scalars) {
    auto input_size = INPUT_SIZE();
    auto output_size = OUTPUT_SIZE;

    // Allocate Memory in Host Memory
    size_t vector_input_size_bytes = sizeof(int) * input_size;
    size_t vector_output_size_bytes = sizeof(int) * output_size;

    std::vector<uint32_t, aligned_allocator<uint32_t> > source_kernel_input(input_size);
    std::vector<uint32_t, aligned_allocator<uint32_t> > source_kernel_output(output_size);
    memset(source_kernel_input.data(), 0, sizeof(uint32_t) * source_kernel_input.size());
    memset(source_kernel_output.data(), 0, sizeof(uint32_t) * source_kernel_output.size());

    // Load points from library representation
    for (int i = 0; i < numPoints(); i++) {
      const int NUM_32B_WORDS_PER_SCALAR = 256 / 32;
      uint32_t *base_ptr = &source_kernel_input[i];

      // load the scalar
      scalar[i].copy_to_fpga_buffer(base_ptr);
      // load the point
      points[i].copy_to_fpga_buffer(base_ptr + NUM_32B_WORDS_PER_SCALAR);
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
    for (unsigned int i = 0; i < devices.size(); i++) {
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
    OCL_CHECK(err,
              cl::Buffer buffer_input(context, CL_MEM_USE_HOST_PTR | CL_MEM_READ_ONLY,
                                      vector_input_size_bytes, source_kernel_input.data(), &err));
    OCL_CHECK(
        err, cl::Buffer buffer_output(context, CL_MEM_USE_HOST_PTR | CL_MEM_WRITE_ONLY,
                                      vector_output_size_bytes, source_kernel_output.data(), &err));

    // Set the "Kernel 0" Arguments
    OCL_CHECK(err, err = krnl_mm2s.setArg(0, buffer_input));
    OCL_CHECK(err, err = krnl_mm2s.setArg(2, input_size));

    // Set the "Kernel 1" Arguments
    OCL_CHECK(err, err = krnl_s2mm.setArg(0, buffer_output));
    OCL_CHECK(err, err = krnl_s2mm.setArg(2, output_size));

    // Copy input data to device global memory
    bench("Copying scalars and points to gmem", [&]() {
      OCL_CHECK(
          err, err = q.enqueueMigrateMemObjects({buffer_input}, 0 /* 0 means from host*/, nullptr));
      OCL_CHECK(err, err = q.finish());
    });

    bench("Doing actual work", [&]() {
      OCL_CHECK(err, err = q.enqueueTask(krnl_mm2s));
      std::cout << "Launched writer kernel!" << std::endl;

      // Launch the reader kernel
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

extern "C" Driver *msm_init(g1_affine_t *rust_points, ssize_t npoints,
                            const std::string &binaryFile) {
  bls12_377_g1::init();
  std::vector<bls12_377_g1::Xyzt> points(npoints);
  for (ssize_t i = 0; i < npoints; i++) {
    std::cout << rust_points[i] << std::endl;
    points[i].copy_from_rust_type(rust_points[i]);
    points[i].println();
  }
  auto *driver = new Driver(points, binaryFile);
  return driver;
}

extern "C" void msm_mult(Driver *driver, g1_projective_t *out, uint64_t batch_size,
                         biginteger256_t *scalars) {
  for (uint64_t i = 0; i < batch_size; i++) {
    printf("Running MSM (Batch %i) with [%i] input points and [%i] output points\n", i,
           driver->numPoints(), NUM_OUTPUT_POINTS);

    driver->feed_msm(out + i, scalars + (i * context->numPoints()));
  }
}
