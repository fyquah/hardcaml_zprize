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

enum class MemoryLayout {
  NORMAL_LAYOUT,
  OPTIMIZED_LAYOUT
};

std::ostream& operator<<(std::ostream &os, CoreType);

std::ostream& operator<<(std::ostream &os, MemoryLayout);

MemoryLayout memory_layout_from_string(std::string);

class NttFpgaDriverArg {
private:
  NttFpgaDriverArg(CoreType core_type, MemoryLayout, uint64_t log_row_size, uint64_t log_blocks);

public:
  const CoreType core_type;
  const MemoryLayout memory_layout;
  const uint64_t log_row_size;
  const uint64_t log_blocks;

  uint64_t row_size();

  uint64_t num_elements();

  static NttFpgaDriverArg create_ntt_2_24(MemoryLayout, uint64_t log_blocks);

  static NttFpgaDriverArg create_ntt_2_18(MemoryLayout, uint64_t log_blocks);

  static NttFpgaDriverArg create_ntt_2_12(MemoryLayout, uint64_t log_blocks);

  static NttFpgaDriverArg create_reverse(MemoryLayout, uint64_t log_row_size, uint64_t log_blocks);
};

class NttFpgaDriver {
public:
  class UserBuffer {
  private:
    friend class NttFpgaDriver;

    cl::Event  ev_transfer_data_to_fpga;
    cl::Event  ev_phase1_work;
    cl::Event  ev_phase2_work;
    cl::Event  ev_transfer_data_from_fpga;
    cl::Buffer cl_buffer_input;
    cl::Buffer cl_buffer_intermediate;
    cl::Buffer cl_buffer_output;
    vec64      host_buffer_input;
    vec64      host_buffer_output;
    bool       in_use;

  public:
    /**
     * Returns a pointer for the user to copy input data to.
     */
    uint64_t* input_data() { return host_buffer_input.data(); }

    /**
     * Returns a pointer for the user to copy output data from. This data
     * buffer will contain validate only after calling [transfer_data_to_fpga]
     */
    uint64_t* output_data() { return host_buffer_output.data(); }
  };

private:
  const CoreType core_type;
  const MemoryLayout memory_layout;
  const uint64_t log_row_size;
  const uint64_t log_blocks;
  const uint64_t row_size;
  const uint64_t matrix_size;
  bool loaded_xclbin;
  const uint64_t m_max_num_buffers_in_flight;

  cl_int err;
  cl::CommandQueue q;
  cl::Context context;
  cl::Kernel krnl_ntt;
  cl::Kernel krnl_controller;
  std::vector<UserBuffer> user_buffers;
  UserBuffer *last_enqueued_buffer;
  
  // A preallocated vector of cl::Event that can be used at runtime.
  std::vector<cl::Event> tmp_events_to_wait_for;

  /* Internal helper functions follows */

  void enqueue_phase1_work(UserBuffer*);

  void enqueue_phase2_work(UserBuffer*);

  void enqueue_transfer_data_to_fpga(UserBuffer*);

  void enqueue_transfer_data_from_fpga(UserBuffer*);

  void set_args(UserBuffer*);

public:
  NttFpgaDriver(NttFpgaDriverArg driver_arg);

  uint64_t input_vector_size() { return matrix_size; }

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
  UserBuffer* request_buffer();

  /**
   * Transfer the NTT input vector to the provided user buffer. Depending
   * on the memory-layout supported by the NTT core, this will either be a
   * simple memcpy or a more complicated data rearrangement.
   */
  void transfer_input_vector_to_user_buffer(UserBuffer*, uint64_t*);

  /**
   * Asynchronously enqueue the buffer for NTT evaluation.
   *
   * Upong calling enqueue_evaluate_async:
   * - the user must not modify the contents of buffer->data_input() as it
   *   is being copied down to the FPGA
   * - the contents of buffer->data_output() can change as the NTT result is
   *   received from the FPGA
   *
   * Calling [enqueue_evaluation_async] in succession without calling
   * [wait_for_result] is not allowed, and can produce undefined behaviour.
   */
  void enqueue_evaluation_async(UserBuffer*);

  /**
   * Poll FPGA for completion of outstanding request on the buffer. This blocks
   * until a result is available from the FPGA for this. If UserBuffer* is not
   * enqueued for any evaluation, behavious is undefined.
   */
  void wait_for_result(UserBuffer*);

  /**
   * Transfer the NTT output vector from the user provided user buffer. To the
   * given data pointer. Depending on the memory-layout supported by the NTT
   * core, this will either be a simple memcpy or a more complicated data
   * rearrangement.
   */
  void transfer_user_buffer_to_output_vector(UserBuffer*, uint64_t*);

  /**
   * Returns the buffer to the driver. This buffer will be free for future
   * requests.
   */
  void free_buffer(UserBuffer*);

  /**
   * A simple synchronous evaluation API.
   *
   * This calls the asynchronous APIs under the hood. This blocks until the
   * NTT is completed.
   */
  void simple_evaluate(uint64_t *output,
                       const uint64_t *input,
                       uint64_t data_length);

  /**
   * Similar to simple_evaluate, but prints to stdout the time taken for various
   * stages of evaluation
   */
  void simple_evaluate_slow_with_profilling(uint64_t *output,
                                            const uint64_t *input,
                                            uint64_t data_length);

  /**
   * Special function for benchmarking. Do not use!
   */
  void expert__transfer_data_to_fpga_blocking(UserBuffer*);

  /**
   * Special function for benchmarking. Do not use!
   */
  void expert__evaluate_on_fpga_blocking(UserBuffer*);

  /**
   * Special function for benchmarking. Do not use!
   */
  void expert__transfer_data_from_fpga_blocking(UserBuffer*);
};

typedef NttFpgaDriver::UserBuffer NttFpgaBuffer;

#endif
