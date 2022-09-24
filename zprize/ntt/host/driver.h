#ifndef DRIVER_H
#define DRIVER_H

#include <algorithm>
#include <chrono>
#include <stdint.h>
#include <vector>

#include <xrt/xrt_device.h>

#include "xcl2.hpp"

#include "gf.h"

typedef std::vector<uint64_t, aligned_allocator<uint64_t>> vec64;

enum class CoreType {
  REVERSE,  // REVERSE is a special core type to debug the HBM memory accessors.
  NTT_2_12,
  NTT_2_18,
  NTT_2_24
};

std::ostream& operator<<(std::ostream &os, CoreType);

class NttFpgaDriverArg {
private:
  NttFpgaDriverArg(CoreType core_type, uint64_t log_row_size);

public:
  const CoreType core_type;
  const uint64_t log_row_size;

  uint64_t row_size();

  uint64_t num_elements();

  static NttFpgaDriverArg create_ntt_2_24();

  static NttFpgaDriverArg create_ntt_2_18();

  static NttFpgaDriverArg create_ntt_2_12();

  static NttFpgaDriverArg create_reverse(uint64_t log_row_size);
};

class NttFpgaDriver {
private:
  const CoreType core_type;
  const uint64_t log_row_size;
  const uint64_t row_size;
  const uint64_t matrix_size;
  cl_int err;
  cl::CommandQueue q;
  cl::Context context;
  cl::Kernel krnl_ntt;
  cl::Kernel krnl_controller;
  cl::Buffer cl_buffer_input;
  cl::Buffer cl_buffer_intermediate;
  cl::Buffer cl_buffer_output;
  vec64 host_buffer_input;
  vec64 host_buffer_output;

public:
  NttFpgaDriver(NttFpgaDriverArg driver_arg);

  void evaluate(uint64_t *output, const uint64_t *intput, uint64_t data_length);

  void load_xclbin(const std::string& binaryFile);
};

#endif
