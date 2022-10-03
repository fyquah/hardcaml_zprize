#include <assert.h>
#include <algorithm>
#include <random>
#include <stdint.h>
#include <vector>

#include "driver.h"
#include "reference.h"

enum class WhatToMeasure {
  MEMCPY_AND_EVALUATE_AND_CHECK,
  EVALUATE_ONLY
};

struct host_args_t {
  std::string binaryFile;
  NttFpgaDriverArg driver_arg;
  uint64_t num_evaluations;
  WhatToMeasure what_to_measure;
};

std::ostream& operator<<(std::ostream& os, WhatToMeasure x) {
  switch (x) {
    case WhatToMeasure::MEMCPY_AND_EVALUATE_AND_CHECK:
      return os << "memcpy-and-evaluate-and-check";
    case WhatToMeasure::EVALUATE_ONLY:
      return os << "evaluate-only";
  }
  return os;
}

static std::vector<std::vector<uint64_t>>
generate_input_vectors(NttFpgaDriver &driver, uint64_t num_examples)
{
  std::mt19937_64 rng;
  std::vector<std::vector<uint64_t>> input_vectors(num_examples);

  for (auto &input_vector : input_vectors)  {
    input_vector.resize(driver.input_vector_size());
    for (size_t j = 0; j < driver.input_vector_size(); j++) {
      input_vector[j] = rng() % MODULUS;
    }
  }

  return input_vectors;
}

static std::vector<std::vector<uint64_t>>
generate_expected_outputs(std::vector<std::vector<uint64_t>>& test_inputs, NttFpgaDriverArg driver_arg)
{
  std::vector<std::vector<uint64_t>> expected_outputs(test_inputs.size());
  for (size_t t = 0; t < expected_outputs.size(); t++) {
    auto &expected_output = expected_outputs[t];
    expected_output.resize(test_inputs[t].size());
    ntt_reference(expected_output.data(), test_inputs[t].data(), driver_arg);
  }
  return expected_outputs;
}

static int
run_ntt_bench_throughput_evaluate_only(NttFpgaDriver &driver,
                                       host_args_t host_args,
                                       std::vector<NttFpgaBuffer*>& user_buffers)
{
  uint64_t num_user_buffers = driver.max_num_buffers_in_flight();

  std::cout << "Generating test data ... " << std::flush;
  auto test_inputs = generate_input_vectors(driver, num_user_buffers);
  auto expected_outputs = generate_expected_outputs(test_inputs, host_args.driver_arg);
  for (uint64_t t = 0; t < num_user_buffers; t++) {
    driver.transfer_input_vector_to_user_buffer(user_buffers[t], test_inputs[t].data());
  }
  std::cout << "Done!" << std::endl;

  std::chrono::time_point<std::chrono::steady_clock> t_start(std::chrono::steady_clock::now());
  size_t t = 0;
  for (uint64_t i = 0; i < host_args.num_evaluations; i++) {
    driver.enqueue_evaluation_async(user_buffers[t]);
    t = (t + 1) % num_user_buffers;
  }
  t = 0;
  for (uint64_t i = 0; i < host_args.num_evaluations; i++) {
    driver.wait_for_result(user_buffers[t]);
    t = (t + 1) % num_user_buffers;
  }
  std::chrono::time_point<std::chrono::steady_clock> t_end(std::chrono::steady_clock::now());

  int failed = 0;
  auto obtained_output = std::vector<uint64_t>(test_inputs.size());
  for (uint64_t t = 0; t < user_buffers.size(); t++) {
    auto *user_buffer = user_buffers[t];
    auto *expected_output = expected_outputs[t].data();
    driver.transfer_user_buffer_to_output_vector(user_buffer, obtained_output.data());

    if (memcmp(expected_output, obtained_output.data(), driver.input_vector_size() * sizeof(uint64_t)) != 0) {
      std::cout << "Incorrect result in buffer " << t << std::endl;
      failed = 1;
    }
  }

  double elapsed_seconds = (std::chrono::duration<double>(t_end - t_start)).count();
  std::cout << "Time taken for " << host_args.num_evaluations << "NTTs" << ": " << elapsed_seconds << "s" << std::endl;
  std::cout << "Amortized time taken per NTT " << (elapsed_seconds / host_args.num_evaluations) << "s" << std::endl;
  std::cout << "Throughput: " << (host_args.num_evaluations / elapsed_seconds) << "NTTs/second" << std::endl;

  return failed;
}

static int
run_ntt_bench_throughput_memcpy_evaluate_and_check(NttFpgaDriver &driver,
                                                   host_args_t host_args,
                                                   std::vector<NttFpgaBuffer*>& user_buffers)
{
  uint64_t num_user_buffers = driver.max_num_buffers_in_flight();

  std::cout << "Generating test data ... " << std::flush;
  auto test_inputs = generate_input_vectors(driver, 16);
  auto expected_outputs = generate_expected_outputs(test_inputs, host_args.driver_arg);
  std::vector<uint64_t> test_vector_indices(num_user_buffers, 0);
  std::vector<uint64_t> obtained_output(driver.input_vector_size());
  std::cout << "Done!" << std::endl;

  int failed = 0;
  auto poll_and_validate = [&](uint64_t t) {
    auto *user_buffer = user_buffers[t];
    auto *expected_output = expected_outputs[test_vector_indices[t]].data();
    driver.transfer_user_buffer_to_output_vector(user_buffer, obtained_output.data());

    driver.wait_for_result(user_buffer);

    if (memcmp(expected_output, obtained_output.data(), driver.input_vector_size() * sizeof(uint64_t)) != 0) {
      std::cout << "Incorrect result in buffer " << t << std::endl;
      failed = 1;
    }
    test_vector_indices[t] = (test_vector_indices[t] + 1) % 16;
  };

  std::chrono::time_point<std::chrono::steady_clock> t_start(std::chrono::steady_clock::now());
  {
    size_t t = 0;
    for (uint64_t i = 0; i < host_args.num_evaluations; i++) {
      if (i >= num_user_buffers) {
        poll_and_validate(t);
      }
      driver.transfer_input_vector_to_user_buffer(
          user_buffers[t],
          test_inputs[test_vector_indices[t]].data());
      driver.enqueue_evaluation_async(user_buffers[t]);
      t = (t + 1) % num_user_buffers;
    }
    if (host_args.num_evaluations < user_buffers.size()) {
      t = 0;
    }
    for (uint64_t i = 0; i < user_buffers.size(); i++) {
      poll_and_validate(t);
      t = (t + 1) % num_user_buffers;
    }
  }
  std::chrono::time_point<std::chrono::steady_clock> t_end(std::chrono::steady_clock::now());

  double elapsed_seconds = (std::chrono::duration<double>(t_end - t_start)).count();
  std::cout << "Time taken for " << host_args.num_evaluations << "NTTs (with memcpy and checking)" << ": " << elapsed_seconds << "s" << std::endl;
  std::cout << "Amortized time taken per NTT " << (elapsed_seconds / host_args.num_evaluations) << "s" << std::endl;
  std::cout << "Throughput: " << (host_args.num_evaluations / elapsed_seconds) << "NTTs/second" << std::endl;

  return failed;
}

static int
run_ntt_bench_throughput(host_args_t host_args)
{
  auto driver_arg = host_args.driver_arg;

  std::cout
    << "Running ntt-fpga throughput benchmark with\n"
    << "  binaryFile =  "  << host_args.binaryFile << "\n"
    << "  core_type "      << driver_arg.core_type << "\n"
    << "  log_row_size = " << driver_arg.log_row_size << "\n"
    << "  num_evaluations = " << host_args.num_evaluations << "\n"
    << "  what_to_measure = "  << host_args.what_to_measure << "\n";

  NttFpgaDriver driver(driver_arg);
  driver.load_xclbin(host_args.binaryFile);

  uint64_t num_user_buffers = driver.max_num_buffers_in_flight();
  std::vector<NttFpgaBuffer*> user_buffers(num_user_buffers);
  for (uint64_t i = 0; i < num_user_buffers; i++) {
    user_buffers[i] = driver.request_buffer();
    assert(user_buffers[i] != nullptr);
  }

  switch (host_args.what_to_measure) {
  case WhatToMeasure::MEMCPY_AND_EVALUATE_AND_CHECK:
    return run_ntt_bench_throughput_memcpy_evaluate_and_check(driver, host_args, user_buffers);

  case WhatToMeasure::EVALUATE_ONLY:
    return run_ntt_bench_throughput_evaluate_only(driver, host_args, user_buffers);

  }

  return 1;
}

// --------------------
// Mostly uninteresting parsing code follows
// --------------------

static const char *flag_xcl_bin_file    = "--xclbin";
static const char *flag_log_row_size    = "--log-row-size";
static const char *flag_core_type       = "--core-type";
static const char *flag_num_evaluations = "--num-evaluations";
static const char *flag_what_to_measure = "--what-to-measure";
static const char *flag_memory_layout   = "--memory-layout";
static const char *flag_log_blocks      = "--log-blocks";

static host_args_t
parse_args(int argc, char **argv)
{
  std::string binaryFile;
  uint64_t log_row_size = 0;
  std::string error_message;
  char *core_type = nullptr;
  char *memory_layout = nullptr;
  uint64_t num_evaluations = 1;
  WhatToMeasure what_to_measure = WhatToMeasure::MEMCPY_AND_EVALUATE_AND_CHECK;
  uint64_t log_blocks = 0;
  
  auto print_usage = [=]() {
    std::cout
      << argv[0] << " "
      << flag_xcl_bin_file << " <FILENAME> " 
      << flag_core_type    << " <REVERSE|NTT> "
      << flag_memory_layout    << " <NORMAL_LAYOUT|OPTIMIZED_LAYOUT> "
      << "[" << flag_log_blocks << " <LOG-BLOCKS>] "
      << "[" << flag_log_row_size << " <LOG-ROW-SIZE>] "
      << "[" << flag_num_evaluations << " <NUM-ROUNDS>] "
      << "[" << flag_what_to_measure << " <memcpy-and-evaluate|evaluate-only>] "
      << std::endl;
  };

  auto capture_next_arg = [&](const char *flag_name) {
    ++argv;
    if (*argv == nullptr || strlen(*argv) == 0) {
      error_message
        .append(flag_name)
        .append(" expects a non-empty argument!");
      throw std::runtime_error(error_message);
    }
    return *argv;
  };

  while (*(++argv) != nullptr) {
    if (strcmp(*argv, flag_xcl_bin_file) == 0) {
      binaryFile = capture_next_arg(flag_xcl_bin_file);
      continue;
    }

    if (strcmp(*argv, flag_log_row_size) == 0) {
      uint64_t parsed = std::stoull(capture_next_arg(flag_log_row_size));
      if (parsed == 0) {
        error_message
          .append(flag_log_row_size)
          .append(" expects a positive numerical argument!");
        throw std::runtime_error(error_message);
      }
      log_row_size = parsed;
      continue;
    }

    if (strcmp(*argv, flag_num_evaluations) == 0) {
      num_evaluations = std::stoull(capture_next_arg(flag_num_evaluations));
      continue;
    }

    if (strcmp(*argv, flag_core_type) == 0) {
      core_type = capture_next_arg(flag_core_type);
      continue;
    }

    if (strcmp(*argv, flag_log_blocks) == 0) {
      log_blocks = std::stoull(capture_next_arg(flag_log_blocks));
      continue;
    }

    if (strcmp(*argv, flag_what_to_measure) == 0) {
      char *s = capture_next_arg(flag_what_to_measure);
      if (strcmp(s, "memcpy-and-evaluate-and-check") == 0) {
        what_to_measure = WhatToMeasure::MEMCPY_AND_EVALUATE_AND_CHECK;

      } else if (strcmp(s, "evaluate-only") == 0) {
        what_to_measure = WhatToMeasure::EVALUATE_ONLY;

      } else {
        error_message.append(flag_what_to_measure);
        error_message.append(" expects one of 'memcpy-and-evaluate-and-check' or 'evaluate-only'");
        throw std::runtime_error(error_message);

      }
      continue;
    }

    if (strcmp(*argv, flag_memory_layout) == 0) {
      memory_layout = capture_next_arg(flag_memory_layout);
      continue;
    }

    print_usage();
    error_message
        .append("Unknown flag: ")
        .append(*argv);
    throw std::runtime_error(error_message);
  }

  if (binaryFile.empty() || core_type == nullptr) {
    print_usage();
    error_message.append("Missing flags?");
    throw std::runtime_error(error_message);
  }

  // resolve the right driver_arg

  NttFpgaDriverArg driver_arg([&]() {
    MemoryLayout parsed_memory_layout = memory_layout_from_string(
        std::string(memory_layout));

    auto throw_if_log_row_size_set = [&]() {
      if (log_row_size) {
        error_message.append(flag_log_row_size);
        error_message.append(" cannot be specified when core_type is NTT-*");
        throw std::runtime_error(error_message);
      }
    };
    if (strcmp(core_type, "NTT-2_12") == 0) {
      throw_if_log_row_size_set();
      return NttFpgaDriverArg::create_ntt_2_12(parsed_memory_layout, log_blocks);

    } else if (strcmp(core_type, "NTT-2_18") == 0) {
      throw_if_log_row_size_set();
      return NttFpgaDriverArg::create_ntt_2_18(parsed_memory_layout, log_blocks);

    } else if (strcmp(core_type, "NTT-2_24") == 0) {
      throw_if_log_row_size_set();
      return NttFpgaDriverArg::create_ntt_2_24(parsed_memory_layout, log_blocks);

    } else if (strcmp(core_type, "REVERSE") == 0) {
      if (!log_row_size) {
        print_usage();
        error_message.append(flag_log_row_size);
        error_message.append(" must be specified as a non-zero value when core_type is REVERSE");
        throw std::runtime_error(error_message);
      }
      return NttFpgaDriverArg::create_reverse(parsed_memory_layout, log_row_size, log_blocks);

    }

    print_usage();
    error_message.append("Unknown --core-type specified!");
    throw std::runtime_error(error_message);
  }());

  host_args_t args = {
      .binaryFile = binaryFile,
      .driver_arg = driver_arg,
      .num_evaluations = num_evaluations,
      .what_to_measure = what_to_measure
      };
  return args;
};

int
main(int argc, char** argv) {
  auto args = parse_args(argc, argv);
  return run_ntt_bench_throughput(args);
}

