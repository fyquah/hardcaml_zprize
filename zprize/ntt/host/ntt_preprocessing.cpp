#include <stdint.h>
#include <string.h>
#include <stdlib.h>

#include <algorithm>
#include <chrono>
#include <vector>

namespace prefetch {
  const uint64_t READ = 0;
  const uint64_t WRITE = 1;
};

#define MEMCPY_64BYTES(dst, in, r, c) \
     memcpy( \
         dst + ((c) * row_size * 8) + ((r) * 8), \
         in + ((c) * 8) + ((r) * row_size), \
         sizeof(uint64_t) * 8 \
         ); 

#define LOGCORES 3
#define NUM_CORES (1 << LOGCORES)

static void inline __attribute__ ((always_inline))
ntt_preprocessing_impl(uint64_t *arg_dst, const uint64_t *arg_in, uint64_t row_size){
  uint64_t *dst = (uint64_t*) __builtin_assume_aligned((void*) arg_dst, 64);
  uint64_t *in  = (uint64_t*) __builtin_assume_aligned((void*) arg_in, 64);

  for (uint64_t block_col = 0; block_col < row_size >> LOGCORES; block_col ++) {
    for (uint64_t row = 0; row < row_size; row += 1) {
      for (uint64_t i = 0; i < NUM_CORES; i++) {
        *(dst++) = in[row * row_size + (block_col * NUM_CORES) + i];
      }
      // MEMCPY_64BYTES(dst, in, r, c + 0);
      // MEMCPY_64BYTES(dst, in, r, c + 1);
      // MEMCPY_64BYTES(dst, in, r, c + 2);
      // MEMCPY_64BYTES(dst, in, r, c + 3);
      // MEMCPY_64BYTES(dst, in, r, c + 4);
      // MEMCPY_64BYTES(dst, in, r, c + 5);
      // MEMCPY_64BYTES(dst, in, r, c + 6);
      // MEMCPY_64BYTES(dst, in, r, c + 7);
    }
  }

  // for (uint64_t r = 0; r < row_size; r += 1) {
  //   for (uint64_t c = 0; c < row_size / 8; c+=8) {
  //     MEMCPY_64BYTES(dst, in, r, c + 0);
  //     MEMCPY_64BYTES(dst, in, r, c + 1);
  //     MEMCPY_64BYTES(dst, in, r, c + 2);
  //     MEMCPY_64BYTES(dst, in, r, c + 3);
  //     MEMCPY_64BYTES(dst, in, r, c + 4);
  //     MEMCPY_64BYTES(dst, in, r, c + 5);
  //     MEMCPY_64BYTES(dst, in, r, c + 6);
  //     MEMCPY_64BYTES(dst, in, r, c + 7);
  //   }
  // }
}

template<uint64_t row_size>
void __attribute__ ((noinline))
ntt_preprocessing_specialized(uint64_t *dst, const uint64_t *in) {
  ntt_preprocessing_impl(dst, in, row_size);
}

void __attribute__ ((noinline))
ntt_preprocessing_unspecialized(uint64_t *dst, const uint64_t *in, uint64_t row_size) {
  ntt_preprocessing_impl(dst, in, row_size);
}

void __attribute__ ((noinline))
ntt_preprocessing(uint64_t *dst, const uint64_t *in, uint64_t row_size) {
  switch (row_size) {
  case 4096:
    return ntt_preprocessing_specialized<4096>(dst, in);
  }

  return ntt_preprocessing_unspecialized(dst, in, row_size);
}
