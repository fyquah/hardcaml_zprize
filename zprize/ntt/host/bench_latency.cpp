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
  uint64_t num_evaluations;
};

static int
run_ntt_test(host_args_t host_args)
{
  auto driver_arg = host_args.driver_arg;
  uint64_t num_elements = driver_arg.num_elements();

  std::cout
    << "Running ntt-fpga latency benchmark with\n"
    << "  binaryFile =  "  << host_args.binaryFile << "\n"
    << "  core_type "      << driver_arg.core_type << "\n"
    << "  log_row_size = " << driver_arg.log_row_size << "\n";

  NttFpgaDriver driver(driver_arg);
  driver.load_xclbin(host_args.binaryFile);

  auto *user_buffer = driver.request_buffer();
  std::vector<uint64_t> expected_output(driver.input_vector_size());

  std::cout << "Setting up input data... " << std::flush;
  {
    std::mt19937_64 rng;
    uint64_t *input_data = user_buffer->input_data();

    // if we have more than 1 test cases, we make the very first test case a special
    // vector full of ones.
    for (size_t i = 0; i < num_elements; i++) {
      input_data[i] = rng() % MODULUS;
    }

    ntt_reference(expected_output.data(), input_data, driver_arg);

    std::cout << "Done!" << std::endl;
  }
  std::vector<double> time_takens(host_args.num_evaluations);

  bool failed = false;
  for (size_t r = 0; r < host_args.num_evaluations; r++) {
    std::chrono::time_point<std::chrono::steady_clock> t_start(std::chrono::steady_clock::now());
    driver.enqueue_evaluation_async(user_buffer);
    driver.wait_for_result(user_buffer);
    std::chrono::time_point<std::chrono::steady_clock> t_end(std::chrono::steady_clock::now());

    double elapsed_seconds = (std::chrono::duration<double>(t_end - t_start)).count();
    time_takens[r] = elapsed_seconds;

    auto *output_data = user_buffer->output_data();

    if (memcmp(expected_output.data(), output_data, driver.input_vector_size() * sizeof(uint64_t)) != 0) {
      std::cout << "Incorrect result in run  " << r << std::endl;
      failed = 1;
    }

  }

  std::sort(time_takens.begin(), time_takens.end());

  std::cout << "Latency over " << host_args.num_evaluations << " NTTs\n";
  std::cout << "Min latency          : " << time_takens[0] << "s\n";
  std::cout << "25-percentile latency: " << time_takens[time_takens.size() / 4] << "s\n";
  std::cout << "Median latency       : " << time_takens[time_takens.size() / 2] << "s\n";
  std::cout << "75-percentile latency: " << time_takens[time_takens.size() * 3 / 4] << "s\n";
  std::cout << "Max latency          : " << time_takens.back() << "s\n";

  return failed;
}

// --------------------
// Mostly uninteresting parsing code follows
// --------------------

static const char *flag_xcl_bin_file    = "--xclbin";
static const char *flag_log_row_size    = "--log-row-size";
static const char *flag_core_type       = "--core-type";
static const char *flag_num_evaluations = "--num-evaluations";

static host_args_t
parse_args(int argc, char **argv)
{
  std::string binaryFile;
  uint64_t log_row_size = 0;
  std::string error_message;
  char *core_type = nullptr;
  uint64_t num_evaluations = 1;
  
  auto print_usage = [=]() {
    std::cout
      << argv[0] << " "
      << flag_xcl_bin_file << " <FILENAME> " 
      << flag_core_type    << " <REVERSE|NTT> "
      << "[" << flag_log_row_size << " <LOG-ROW-SIZE>] "
      << "[" << flag_num_evaluations << " <NUM-ROUNDS>] "
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
      .num_evaluations = num_evaluations 
      };
  return args;
};

int
main(int argc, char** argv) {
  auto args = parse_args(argc, argv);
  return run_ntt_test(args);
}

