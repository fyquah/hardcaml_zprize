#include <assert.h>

#include "driver.h"

std::ostream& operator<<(std::ostream &os, CoreType core_type)
{
  switch (core_type) {
  case CoreType::REVERSE:
    return os << "REVERSE";

  case CoreType::NTT_2_12:
    return os << "NTT_2_12";

  case CoreType::NTT_2_18:
    return os << "NTT_2_18";

  case CoreType::NTT_2_24:
    return os << "NTT_2_24";
  }

  assert(false);
}


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


NttFpgaDriverArg::NttFpgaDriverArg(CoreType core_type, uint64_t log_row_size)
  : core_type(core_type), log_row_size(log_row_size)
{
}

uint64_t NttFpgaDriverArg::row_size() {
  return 1ull << log_row_size; 
}

uint64_t NttFpgaDriverArg::num_elements() {
  return row_size() * row_size();
}

NttFpgaDriverArg NttFpgaDriverArg::create_reverse(uint64_t log_row_size)
{
  return NttFpgaDriverArg(CoreType::REVERSE, log_row_size);
}

NttFpgaDriverArg NttFpgaDriverArg::create_ntt_2_24()
{
  return NttFpgaDriverArg(CoreType::NTT_2_24, 12);
}

NttFpgaDriverArg NttFpgaDriverArg::create_ntt_2_18()
{
  return NttFpgaDriverArg(CoreType::NTT_2_18, 9);
}

NttFpgaDriverArg NttFpgaDriverArg::create_ntt_2_12()
{
  return NttFpgaDriverArg(CoreType::NTT_2_12, 6);
}


NttFpgaDriver::NttFpgaDriver(NttFpgaDriverArg driver_arg) : 
    core_type(driver_arg.core_type),
    log_row_size(driver_arg.log_row_size),
	  row_size(1 << driver_arg.log_row_size),
	  matrix_size(row_size * row_size),
	  host_buffer_points(matrix_size),
    host_buffer_intermediate(matrix_size)
{
}


template<typename F>
static auto
bench(const char *descr, F f) {
  LogTimeTaken log_time_taken(descr);
  return f();
}


void NttFpgaDriver::evaluate(uint64_t *out, const uint64_t *in, uint64_t data_length) {
  if (data_length > matrix_size) {
    std::string error_message;
    error_message
        .append("Data length exceeds supported size of driver. data_length = ")
        .append(std::to_string(data_length))
        .append(", driver log_row_size = ")
        .append(std::to_string(log_row_size));
    throw std::runtime_error(error_message);
  }

  bench("Evaluate NTT", [&]() {

    bench("Copy to internal page-aligned buffer", [&](){
        memcpy(host_buffer_points.data()              , in, sizeof(uint64_t) * data_length);
        memset(host_buffer_points.data() + data_length, 0 , sizeof(uint64_t) * (matrix_size - data_length));
    });

    bench("Copying input points to device", [&]() {
        OCL_CHECK(err, err = q.enqueueMigrateMemObjects({cl_buffer_points}, 0 /* 0 means from host*/, nullptr, nullptr));
        OCL_CHECK(err, err = q.finish());
    });

    bench("Doing phase 1 work (including twiddling)", [&]() {
      OCL_CHECK(err, err = krnl_controller.setArg(5, (uint8_t) 0b0));
      // The reverse core uses the C++ HLS ap_ctrl_handshake mechanism, hence it needs to be
      // explicitly enqueued. The NTT core operates solely based on axi streams without any
      // control signals., so we don't need to enqueue it.
      if (core_type == CoreType::REVERSE) {
        OCL_CHECK(err, err = q.enqueueTask(krnl_ntt));
      }
      OCL_CHECK(err, err = q.enqueueTask(krnl_controller));
      OCL_CHECK(err, err = q.finish());
    });

    bench("Doing phase 2 work", [&]() {
      OCL_CHECK(err, err = krnl_controller.setArg(5, (uint8_t) 0b1));
      // See comment in "Doing phase 1 work"
      if (core_type == CoreType::REVERSE) {
        OCL_CHECK(err, err = q.enqueueTask(krnl_ntt));
      }
      OCL_CHECK(err, err = q.enqueueTask(krnl_controller));
      OCL_CHECK(err, err = q.finish());
    });

    bench("Copying final result to host", [&]() {
      OCL_CHECK(err, err = q.enqueueMigrateMemObjects({cl_buffer_points}, CL_MIGRATE_MEM_OBJECT_HOST));
      OCL_CHECK(err, err = q.finish());
    });

    bench("Copy from internal page-aligned buffer", [&]() {
      memcpy(out, host_buffer_points.data(), sizeof(uint64_t) * data_length);
    });
  });
}

void NttFpgaDriver::load_xclbin(const std::string& binaryFile)
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
          std::cout << "Device[" << i << "]: program successful!" << std::endl;
          if (core_type == CoreType::REVERSE) {
            OCL_CHECK(err, krnl_ntt = cl::Kernel(program, "krnl_ntt", &err));
          }
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
                          CL_MEM_USE_HOST_PTR | CL_MEM_READ_WRITE,
                          vector_size_bytes,
                          host_buffer_intermediate.data(),
                          &err));

  // Set the controller arguments
  OCL_CHECK(err, err = krnl_controller.setArg(2, cl_buffer_points));
  OCL_CHECK(err, err = krnl_controller.setArg(3, cl_buffer_intermediate));
  OCL_CHECK(err, err = krnl_controller.setArg(4, (uint16_t) row_size));

  // Set the reverse arguments
  if (core_type == CoreType::REVERSE) {
    OCL_CHECK(err, err = krnl_ntt.setArg(2, (uint16_t) row_size));
  }
}
