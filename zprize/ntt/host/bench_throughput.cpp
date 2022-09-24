#include <assert.h>
#include <algorithm>
#include <random>
#include <stdint.h>
#include <vector>

#include "driver.h"
#include "reference.h"

struct host_args_t {
  std::string binaryFile;
  NttFpgaDriverArg driver_arg;
  uint64_t num_test_cases;
};

static int
run_ntt_test(host_args_t host_args)
{
  auto driver_arg = host_args.driver_arg;
  uint64_t num_elements = driver_arg.num_elements();

  std::cout
    << "Running ntt-fpga throughput benchmark with\n"
    << "  binaryFile =  "  << host_args.binaryFile << "\n"
    << "  core_type "      << driver_arg.core_type << "\n"
    << "  log_row_size = " << driver_arg.log_row_size << "\n";

  NttFpgaDriver driver(driver_arg);
  driver.load_xclbin(host_args.binaryFile);

  std::mt19937_64 rng;
  std::vector<NttFpgaBuffer*> user_buffers(driver.max_num_buffers_in_flight());
  std::vector<std::vector<uint64_t>> expected_outputs(driver.max_num_buffers_in_flight());
  for (uint64_t t = 0; t < user_buffers.size(); t++) {
    user_buffers[t] = driver.request_buffer();
    assert(user_buffers[t] != nullptr);
  }

  for (uint64_t t = 0; t < user_buffers.size(); t++) {
    auto *user_buffer = user_buffers[t];
    auto &expected_output = expected_outputs[t];
    expected_output.resize(driver.input_vector_size());

    uint64_t *input_data = user_buffer->input_data();

    // if we have more than 1 test cases, we make the very first test case a special
    // vector full of ones.
    if (host_args.num_test_cases == 1 && t == 0) {
      for (size_t i = 0; i < num_elements; i++) {
        input_data[i] = 1;
      }
    } else {
      for (size_t i = 0; i < num_elements; i++) {
        input_data[i] = rng() % MODULUS;
      }
    }

    ntt_reference(expected_output.data(), input_data, driver_arg);
  }

  std::chrono::time_point<std::chrono::steady_clock> t_start(std::chrono::steady_clock::now());
  for (size_t t = 0; t < driver.max_num_buffers_in_flight(); t++) {
    driver.enqueue_evaluation_async(user_buffers[t]);
  }
  for (size_t t = 0; t < driver.max_num_buffers_in_flight(); t++) {
    driver.wait_for_result(user_buffers[t]);
  }
  std::chrono::time_point<std::chrono::steady_clock> t_end(std::chrono::steady_clock::now());

  double elapsed_seconds = (t_end - t_start).count();
  uint64_t num_ntts = driver.max_num_buffers_in_flight();

  std::cout << "Time taken for " << num_ntts << "NTTs" << ": " << elapsed_seconds << "s" << std::endl;
  std::cout << "Amortized time taken per NTT " << (elapsed_seconds / num_ntts) << std::endl;
  std::cout << "Throughput: " << (num_ntts / elapsed_seconds) << "NTTs/second" << std::endl;

  return 0;
}

// --------------------
// Mostly uninteresting parsing code follows
// --------------------

static const char *flag_xcl_bin_file   = "--xclbin";
static const char *flag_log_row_size   = "--log-row-size";
static const char *flag_core_type      = "--core-type";
static const char *flag_num_test_cases = "--num-test-cases";

static host_args_t
parse_args(int argc, char **argv)
{
  std::string binaryFile;
  uint64_t log_row_size = 0;
  std::string error_message;
  char *core_type = nullptr;
  uint64_t num_test_cases = 1;
  
  auto print_usage = [=]() {
    std::cout
      << argv[0] << " "
      << flag_xcl_bin_file << " <FILENAME> " 
      << flag_core_type    << " <REVERSE|NTT> "
      << "[" << flag_log_row_size << " <LOG-ROW-SIZE>] "
      << "[" << flag_num_test_cases << " <NUM-TEST-CASES>] "
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

    if (strcmp(*argv, flag_num_test_cases) == 0) {
      num_test_cases = std::stoull(capture_next_arg(flag_num_test_cases));
      continue;
    }

    if (strcmp(*argv, flag_core_type) == 0) {
      core_type = capture_next_arg(flag_core_type);
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
    auto throw_if_log_row_size_set = [&]() {
      if (log_row_size) {
        error_message.append(flag_log_row_size);
        error_message.append(" cannot be specified when core_type is NTT-*");
        throw std::runtime_error(error_message);
      }
    };
    if (strcmp(core_type, "NTT-2_12") == 0) {
      throw_if_log_row_size_set();
      return NttFpgaDriverArg::create_ntt_2_12();

    } else if (strcmp(core_type, "NTT-2_18") == 0) {
      throw_if_log_row_size_set();
      return NttFpgaDriverArg::create_ntt_2_18();

    } else if (strcmp(core_type, "NTT-2_24") == 0) {
      throw_if_log_row_size_set();
      return NttFpgaDriverArg::create_ntt_2_24();

    } else if (strcmp(core_type, "REVERSE") == 0) {
      if (!log_row_size) {
        print_usage();
        error_message.append(flag_log_row_size);
        error_message.append(" must be specified as a non-zero value when core_type is REVERSE");
        throw std::runtime_error(error_message);
      }
      return NttFpgaDriverArg::create_reverse(log_row_size);

    }

    print_usage();
    error_message.append("Unknown --core-type specified!");
    throw std::runtime_error(error_message);
  }());

  host_args_t args = {
      .binaryFile = binaryFile,
      .driver_arg = driver_arg,
      .num_test_cases = num_test_cases
      };
  return args;
};

int
main(int argc, char** argv) {
  auto args = parse_args(argc, argv);
  return run_ntt_test(args);
}

