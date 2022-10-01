#include "ntt_postprocessing.h"

#define LOGCORES 3
#define NUM_CORES (1 << LOGCORES)

void __attribute__ ((noinline))
ntt_postprocessing(uint64_t *arg_dst,
                   const uint64_t *arg_in,
                   uint64_t row_size,
                   uint64_t logblocks) {
  uint64_t num_blocks = (1 << logblocks);

  uint64_t pos = 0;

  for (uint64_t col1 = 0; col1 < (row_size >> (LOGCORES + logblocks)); col1++) {
    for (uint64_t row = 0; row < row_size; row++) {
      for (uint64_t col2 = 0; col2 < (1ull << (LOGCORES + logblocks)); col2++) {
        arg_dst[(row_size * row) + ((col1 * NUM_CORES * num_blocks) + col2)] = arg_in[pos++];
      }
    }
  }
}

