#include <stdint.h>
#include <string.h>
#include <stdlib.h>

#include <iostream>
#include <algorithm>
#include <chrono>
#include <vector>

#include "host_to_fpga_transpose.h"

static void __attribute__ ((noinline))
naive_reference(uint64_t *dst, uint64_t *in, uint64_t row_size){
  for (uint64_t c = 0; c < row_size / 8; c++) {
    for (uint64_t r = 0; r < row_size; r++) {
      for (uint64_t i = 0; i < 8; i++) {
        *dst = in[(r * row_size) + (c * 8) + i];
        dst++;
      }
    }
  }
}

static void __attribute__ ((noinline))
pretend_to_be_useful(uint64_t *dst) {
  (void) dst;
}

int main()
{
  uint64_t row_size = 4096;
  uint64_t *in  = (uint64_t*) aligned_alloc(64, row_size * row_size * sizeof(uint64_t));
  uint64_t *dst_opt = (uint64_t*) aligned_alloc(64, row_size * row_size * sizeof(uint64_t));
  uint64_t *dst_naive = (uint64_t*) aligned_alloc(64, row_size * row_size * sizeof(uint64_t));

  for (uint64_t i = 0; i < (row_size * row_size); i++) {
    in[i] = i;
  }

  host_to_fpga_transpose(dst_opt  , in, row_size);
  naive_reference(dst_naive, in, row_size);

  if (memcmp(dst_naive, dst_opt, sizeof(uint64_t) * row_size * row_size) != 0) {
    printf("naive and optimized mismatch\n");
    fflush(stdout);
    return 1;
  }

  std::vector<double> time_takens;
  uint64_t num_runs = 100;

  for (uint64_t i = 0; i < num_runs; i++) {
    std::chrono::time_point<std::chrono::steady_clock> t_start(std::chrono::steady_clock::now());
    host_to_fpga_transpose(dst_opt, in, row_size);
    std::chrono::time_point<std::chrono::steady_clock> t_end(std::chrono::steady_clock::now());
    time_takens.push_back((std::chrono::duration<double>(t_end - t_start)).count());
  }

  pretend_to_be_useful(dst_opt);
  pretend_to_be_useful(dst_naive);

  std::sort(time_takens.begin(), time_takens.end());

  std::cout << "Latency over " << num_runs << " copies\n";
  std::cout << "Min latency          : " << time_takens[0] << "s\n";
  std::cout << "25-percentile latency: " << time_takens[time_takens.size() / 4] << "s\n";
  std::cout << "Median latency       : " << time_takens[time_takens.size() / 2] << "s\n";
  std::cout << "75-percentile latency: " << time_takens[time_takens.size() * 3 / 4] << "s\n";
  std::cout << "Max latency          : " << time_takens.back() << "s\n";
}
