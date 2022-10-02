#include <algorithm>
#include <fstream>
#include <iomanip>
#include <optional>
#include <random>
#include <stdint.h>
#include <vector>

#include "driver.h"
#include "reference.h"

struct host_args_t {
  std::string binaryFile;
  NttFpgaDriverArg driver_arg;
  std::string input_filename;
  std::string output_filename;
};

static void
load_input(vec64& data, const std::string& filename)
{
  std::string error_message;
  uint64_t i = 0;
  uint64_t x;
  std::ifstream fin(filename);

  if (!fin.is_open()) {
    error_message.append("Failed to open input_file \"");
    error_message.append(filename);
    error_message.append("\" Aborting!");
    throw std::runtime_error(error_message);
  }

  while (fin >> std::hex >> x) {
    if (i >= data.size()) {
      error_message.append("Expecting exactly ");
      error_message.append(std::to_string(data.size()));
      error_message.append(" elements, but got more?");
      throw std::runtime_error(error_message);
    }

    data[i++] = x;
  }

  if (i != data.size()) {
    error_message.append("Expecting exactly ");
    error_message.append(std::to_string(data.size()));
    error_message.append(" elements, in input file ");
    error_message.append(filename);
    error_message.append(", but only got ");
    error_message.append(std::to_string(i));
    error_message.append("?");
    throw std::runtime_error(error_message);
  }
}

static void
store_output(const vec64& data, const std::string& filename)
{
  std::string error_message;
  std::ofstream fout(filename);

  if (!fout.is_open()) {
    error_message.append("Failed to open output_file \"");
    error_message.append(filename);
    error_message.append("\" Aborting!");
    throw std::runtime_error(error_message);
  }

  for (uint64_t x : data) {
    fout << "0x" << std::hex << std::setw(16) << std::setfill('0') << x << "\n";
  }

  fout.close();
}


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
  vec64 input(num_elements);
  vec64 output(num_elements);

  load_input(input, host_args.input_filename);

  driver.load_xclbin(host_args.binaryFile);
  driver.simple_evaluate_slow_with_profilling(output.data(), input.data(), num_elements);

  store_output(output, host_args.output_filename);

  return 0;
}

// --------------------
// Mostly uninteresting parsing code follows
// --------------------

static const char *flag_xcl_bin_file    = "--xclbin";
static const char *flag_log_row_size    = "--log-row-size";
static const char *flag_input_filename  = "--input-filename";
static const char *flag_output_filename = "--output-filename";
static const char *flag_core_type       = "--core-type";
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
  char *input_filename = nullptr;
  char *output_filename = nullptr;
  uint64_t log_blocks = 0;
  
  auto print_usage = [=]() {
    std::cout
      << argv[0] << " "
      << flag_xcl_bin_file << " <FILENAME> " 
      << flag_core_type    << " <REVERSE|NTT> "
      << flag_memory_layout    << " <NORMAL_LAYOUT|OPTIMIZED_LAYOUT> "
      << "[" << flag_log_row_size << " <LOG-ROW-SIZE>] "
      << "[" << flag_log_blocks << " <LOG-BLOCKS>] "
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

    if (strcmp(*argv, flag_log_blocks) == 0) {
      log_blocks = std::stoull(capture_next_arg(flag_log_blocks));
      continue;
    }

    if (strcmp(*argv, flag_core_type) == 0) {
      core_type = capture_next_arg(flag_core_type);
      continue;
    }
    if (strcmp(*argv, flag_input_filename) == 0) {
      input_filename = capture_next_arg(flag_input_filename);
      continue;
    }

    if (strcmp(*argv, flag_output_filename) == 0) {
      output_filename = capture_next_arg(flag_output_filename);
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
      .input_filename = input_filename,
      .output_filename = output_filename,
      };
  return args;
};

int
main(int argc, char** argv) {
  auto args = parse_args(argc, argv);
  return run_ntt_test(args);
}
