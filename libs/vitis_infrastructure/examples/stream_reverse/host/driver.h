#include <algorithm>
#include <chrono>
#include <stdint.h>
#include <vector>

#include <xrt/xrt_device.h>

#include "xcl2.hpp"

typedef std::vector<uint64_t, aligned_allocator<uint64_t>> vec64;

class LogTimeTaken {
private:
  const char *descr;
  std::chrono::time_point<std::chrono::steady_clock> start;

public:
  LogTimeTaken(const char *descr)
    : descr(descr), start(std::chrono::steady_clock::now()) {}

  ~LogTimeTaken() {
    auto end = std::chrono::steady_clock::now();
    std::chrono::duration<double> elapsed_seconds = end - start;
    std::cout << "[" << descr << "] " << elapsed_seconds.count() << "s\n";
  }
};

template<typename F>
auto bench(const char *descr, F f) {
  LogTimeTaken log_time_taken(descr);
  return f();
}

class NttFpgaDriver {
private:
  const uint64_t row_size;
  const uint64_t matrix_size;
  cl_int err;
  cl::CommandQueue q;
  cl::Context context;
  cl::Kernel krnl_reverse;
  cl::Kernel krnl_controller;
  cl::Buffer cl_buffer_points;
  cl::Buffer cl_buffer_intermediate;
  vec64 host_buffer_points;

public:
  NttFpgaDriver(uint64_t row_size) :
	  row_size(row_size),
	  matrix_size(row_size * row_size),
	  host_buffer_points(matrix_size)
  {
  }

  void evaluate_inplace(uint64_t *data) {
    bench("Copy to internal page-aligned buffer", [&](){
        memcpy(host_buffer_points.data(), data, sizeof(uint64_t) * matrix_size);
    });

    // Copy buffer input to device memory
    bench("copy to device", [&]() {
        OCL_CHECK(err, err = q.enqueueMigrateMemObjects({cl_buffer_points}, 0 /* 0 means from host*/, nullptr, nullptr));
        OCL_CHECK(err, err = q.finish());
    });

    // Actually do computation
    bench("Doing actual work", [&]() {
	OCL_CHECK(err, err = q.enqueueTask(krnl_reverse));
	OCL_CHECK(err, err = q.enqueueTask(krnl_controller));
	OCL_CHECK(err, err = q.finish());
    });

    // Copy Result from Device Global Memory to Host Local Memory
    bench("copy from device", [&]() {
        OCL_CHECK(err, err = q.enqueueMigrateMemObjects({cl_buffer_points}, CL_MIGRATE_MEM_OBJECT_HOST));
        OCL_CHECK(err, err = q.finish());
    });

    bench("Copy from internal page-aligned buffer", [&]() {
        memcpy(data, host_buffer_points.data(), sizeof(uint64_t) * matrix_size);
    });
  }

  void load_xclbin(const std::string& binaryFile)
  {
    // Allocate Memory in Host Memory
    size_t size = row_size * row_size;
    size_t vector_size_bytes = sizeof(uint64_t) * size;
    host_buffer_points = vec64(size);

    // OPENCL HOST CODE AREA START
    // Create Program and Kernel
    auto devices = xcl::get_xil_devices();
    auto device = devices[0];

    // read_binary_file() is a utility API which will load the binaryFile
    // and will return the pointer to file buffer.
    auto fileBuf = xcl::read_binary_file(binaryFile);
    cl::Program::Binaries bins{{fileBuf.data(), fileBuf.size()}};
    bool valid_device = false;
    for (size_t i = 0; i < devices.size(); i++) {
        auto device = devices[i];
        // Creating Context and Command Queue for selected Device
        OCL_CHECK(err, context = cl::Context(device, nullptr, nullptr, nullptr, &err));
        OCL_CHECK(err, q = cl::CommandQueue(context, device, CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE | CL_QUEUE_PROFILING_ENABLE, &err));

        std::cout << "Trying to program device[" << i << "]: " << device.getInfo<CL_DEVICE_NAME>() << std::endl;
        cl::Program program(context, {device}, bins, nullptr, &err);
        if (err != CL_SUCCESS) {
            std::cout << "Failed to program device[" << i << "] with xclbin file!\n";
        } else {
            std::cout << "Device[" << i << "]: program successful!\n";
            OCL_CHECK(err, krnl_reverse = cl::Kernel(program, "krnl_reverse", &err));
            OCL_CHECK(err, krnl_controller = cl::Kernel(program, "krnl_controller", &err));
            valid_device = true;
            break; // we break because we found a valid device
        }
    }
    if (!valid_device) {
        std::cout << "Failed to program any device found, exit!\n";
        exit(EXIT_FAILURE);
    }

    // Allocate Buffer in Global Memory
    OCL_CHECK(err, cl_buffer_points = cl::Buffer(
                            context,
                            CL_MEM_USE_HOST_PTR | CL_MEM_READ_WRITE,
                            vector_size_bytes,
                            host_buffer_points.data(),
                            &err)
                    );
    OCL_CHECK(err, cl_buffer_intermediate = cl::Buffer(
                            context,
                            CL_MEM_READ_WRITE,
                            vector_size_bytes,
                            nullptr,
                            &err));

    // Set the controller arguments
    OCL_CHECK(err, err = krnl_controller.setArg(2, cl_buffer_points));
    OCL_CHECK(err, err = krnl_controller.setArg(3, cl_buffer_intermediate));
    OCL_CHECK(err, err = krnl_controller.setArg(4, (uint16_t) row_size));

    // Set the reverse arguments
    OCL_CHECK(err, err = krnl_reverse.setArg(2, (uint16_t) row_size));
  }
};

