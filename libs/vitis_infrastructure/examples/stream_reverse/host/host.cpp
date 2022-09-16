#include <algorithm>
#include <stdint.h>
#include <vector>

#include "driver.h"

static void
transpose(uint64_t *p, uint64_t row_size)
{
  for (uint64_t i = 0; i < row_size; i++) {
    for (uint64_t j = i + 1; j < row_size; j++) {
      std::swap(p[(i * row_size) + j], p[(j * row_size) + i]);
    }
  }
}

static void
do_stuff(uint64_t *x, uint64_t row_size) {
  std::reverse(x, x+row_size);
}

static vec64
compute_expected(const vec64& src, uint64_t row_size)
{
  vec64 dst(src.begin(), src.end());

  // phase 1, read transpose, write transpose
  transpose(dst.data(), row_size);
  for (size_t i = 0; i < row_size; i++) {
    do_stuff(dst.data() + (i * row_size), row_size);
  }
  transpose(dst.data(), row_size);

  // phase 1, read linear, write transpose
  for (size_t i = 0; i < row_size; i++) {
    do_stuff(dst.data() + (i * row_size), row_size);
  }
  transpose(dst.data(), row_size);

  return dst;
}

static int
test_streaming(const std::string& binaryFile, const uint16_t row_size)
{
    NttFpgaDriver driver(row_size);
    vec64 points(row_size * row_size);

    driver.load_xclbin(binaryFile);

    for (size_t i = 0; i < row_size * row_size; i++)
        points[i] = i;

    auto expected_output = compute_expected(points, row_size);

    driver.evaluate_inplace(points.data());

    // Compare the results of the Device to the simulation
    int match = 0;
    for (size_t i = 0; i < row_size * row_size; i++) {
        if (expected_output[i] != points[i]) {
            std::cout << "Error: Result mismatch" << std::endl;
            std::cout << "i = " << i
		      << " Expected = " << expected_output[i]
                      << " Obtained = " << points[i]
		      << std::endl;
            match = 1;
            break;
        }
    }

    std::cout << "STREAMING TEST " << (match ? "FAILED" : "PASSED") << std::endl;
    return match;
}

int
main(int argc, char** argv) {
    if (argc != 3) {
        std::cout << "Usage: " << argv[0] << " <XCLBIN File> <LOG ROW SIZE>" << std::endl;
        return EXIT_FAILURE;
    }

    int res = 0;
    std::string binaryFile = argv[1];
    uint64_t log_row_size = atoi(argv[2]);
    res |= test_streaming(binaryFile, 1 << log_row_size);

    return res;
}
