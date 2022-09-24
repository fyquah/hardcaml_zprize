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

class NttFpgaDriver;

class NttFpgaBuffer {
private:
  uint64_t       *m_input_data;
  uint64_t       *m_output_data;
  bool            m_in_use;
  const uint16_t  m_index;

  friend class NttFpgaDriver;

public:
  NttFpgaBuffer(uint64_t *arg_input_data, uint64_t *arg_output_data, uint32_t arg_index)
    : m_input_data(arg_input_data), m_output_data(arg_output_data), m_index(arg_index)
  {};

  NttFpgaBuffer() = delete;
  NttFpgaBuffer(const NttFpgaBuffer &) = delete;
  NttFpgaBuffer(NttFpgaBuffer &&) = default;

  /**
   * Return the internal data pointer.
   */
  uint64_t* input_data() { return m_input_data; }

  uint64_t* output_data() { return m_output_data; }
};

class NttFpgaDriver {
private:
  struct Events {
    cl::Event transfer_data_to_fpga;
    cl::Event phase1_work;
    cl::Event phase2_work;
    cl::Event transfer_data_from_fpga;
  };

  const CoreType core_type;
  const uint64_t log_row_size;
  const uint64_t row_size;
  const uint64_t matrix_size;
  bool loaded_xclbin;
  const uint64_t m_max_num_buffers_in_flight;

  cl_int err;
  cl::CommandQueue q;
  cl::Context context;
  cl::Kernel krnl_ntt;
  cl::Kernel krnl_controller;
  std::vector<cl::Buffer> cl_buffer_inputs;
  std::vector<cl::Buffer> cl_buffer_intermediate;
  std::vector<cl::Buffer> cl_buffer_outputs;
  std::vector<vec64> host_buffer_inputs;
  std::vector<vec64> host_buffer_outputs;
  std::vector<NttFpgaBuffer> user_buffers;
  std::vector<Events> internal_cl_events;
  cl::Event outstanding_execution;

  void enqueue_for_phase_1(NttFpgaBuffer*);

  void enqueue_for_phase_2(NttFpgaBuffer*);

  Events& events_for_buffer(NttFpgaBuffer* buffer) {
    return internal_cl_events.at(buffer->m_index);
  }

public:
  NttFpgaDriver(NttFpgaDriverArg driver_arg);

  /**
   * Loads the xclbin into the FPGA. This needs to be called before any
   * evaluation.
   *
   * Note that this function does not verify that the binaryFile matches
   * the provided driver_arg. The caller must make sure that are using the
   * right .xclbin. Failing to do so will result in incorrect results /
   * deadlocks
   */
  void load_xclbin(const std::string& binaryFile);

  /**
   * Returns the number of parallel buffers that can be used simultaneously.
   */
  const uint64_t max_num_buffers_in_flight() {
    return m_max_num_buffers_in_flight;
  }

  /**
   * Requests a free buffer to be used for NTT evaluation. Returns pointer to
   * a buffer is available, returns NULL if if there is no free buffers
   * available.
   */
  NttFpgaBuffer* request_buffer();

  /**
   * Enqueue a buffer for evaluation.
   *
   * Raises an exception if the buffer is already evaluating. This will enqueue
   * the operation asynchronously.
   */
  void transfer_data_to_fpga(NttFpgaBuffer*);

  void enqueue_for_evaluation_async(NttFpgaBuffer*);

  /**
   * Poll the buffer for completion. This blocks until a result is available
   * from the FPGA for this. If NttFpgaBuffer* is not enqueued for any
   * evaluation, this raises an exception.
   */
  void poll_for_completion_blocking(NttFpgaBuffer*);

  /**
   * Wait for the NTT evaluation to complete and copy data from the FPGA back
   * to the host. This blocks until the copy is done.
   */
  void transfer_data_from_fpga_blocking(NttFpgaBuffer*);

  /**
   * Returns the buffer to the driver. This buffer will be free for future
   * requests.
   */
  void free_buffer(NttFpgaBuffer*);

  void simple_evaluate(uint64_t *output,
                       const uint64_t *input,
                       uint64_t data_length);

  void simple_evaluate_slow_with_profilling(uint64_t *output,
                                            const uint64_t *input,
                                            uint64_t data_length);
};

#endif
