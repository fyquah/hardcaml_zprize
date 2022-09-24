#include <algorithm>
#include <optional>
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
    << "Running ntt-fpga test with\n"
    << "  binaryFile =  "  << host_args.binaryFile << "\n"
    << "  core_type "      << driver_arg.core_type << "\n"
    << "  log_row_size = " << driver_arg.log_row_size << "\n";

  NttFpgaDriver driver(driver_arg);
  driver.load_xclbin(host_args.binaryFile);

  std::mt19937_64 rng;
  vec64 input(num_elements);
  vec64 expected_output(num_elements);
  vec64 obtained_output(num_elements);
  std::vector<bool> test_cases_okay(host_args.num_test_cases, true);

  for (size_t t = 0; t < host_args.num_test_cases; t++) {

    std::cout << "Generating randomized vector..\n";

    // if we have more than 1 test cases, we make the very first test case a special
    // vector full of ones.
    if (host_args.num_test_cases == 1 && t == 0) {
      for (size_t i = 0; i < num_elements; i++) {
        input[i] = 1;
      }
    } else {
      for (size_t i = 0; i < num_elements; i++) {
        input[i] = rng() % MODULUS;
      }
    }

    std::cout << "Computing reference output..\n";
    ntt_reference(expected_output.data(), input.data(), driver_arg);

    uint64_t num_runs = 3;
    if (const char* unused_xcl_emulation_mode = std::getenv("XCL_EMULATION_MODE")) {
      (void) unused_xcl_emulation_mode;
      std::cout << "Since in a simulation, onlly running once!\n";
      num_runs = 1;
    }

    for (uint64_t run = 0; run < num_runs; run++) {
      std::cout << "Run " << run << ": ";
      driver.simple_evaluate(obtained_output.data(), input.data(), num_elements);

      // Compare the results of the Device to the simulation
      int found_mismatch = 0;
      for (size_t i = 0; i < num_elements; i++) {
        if (expected_output[i] != obtained_output[i]) {
          std::cout << "Error: Result mismatch" << std::endl;
          std::cout
            << " i = " << i
            << " Expected = " << expected_output[i]
            << " Obtained = " << obtained_output[i]
            << std::endl;
          found_mismatch = 1;
          break;
        }
      }
      if (found_mismatch) {
        test_cases_okay[t] = false;
      } else {
        std::cout << "Ok!";
      }

      std::cout << "\n";
    }

  }

  bool all_okay = true;
  for (bool t : test_cases_okay) {
    all_okay = all_okay && t;
  }

  for (uint64_t t = 0; t < host_args.num_test_cases ; t++) {
    std::cout
      << "Test case[" << t << "]: "
      << (test_cases_okay[t] ? "PASSED" : "FAILED")
      << "\n";
  }

  std::cout << "\nNTT TEST " << (all_okay ? "PASSED" : "FAILED") << std::endl;
  return (all_okay ? 0 : 1);
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
