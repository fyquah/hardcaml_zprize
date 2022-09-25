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

const uint64_t prefetch_lookahead = 8;

static void inline __attribute__ ((always_inline))
host_to_fpga_transpose_impl(uint64_t *arg_dst, const uint64_t *arg_in, uint64_t row_size){
  uint64_t *dst = (uint64_t*) __builtin_assume_aligned((void*) arg_dst, 64);
  uint64_t *in  = (uint64_t*) __builtin_assume_aligned((void*) arg_in, 64);

  if (prefetch_lookahead == 0) {
    for (uint64_t r = 0; r < row_size; r += 1) {
      for (uint64_t c = 0; c < row_size / 8; c+=8) {
        MEMCPY_64BYTES(dst, in, r, c + 0);
        MEMCPY_64BYTES(dst, in, r, c + 1);
        MEMCPY_64BYTES(dst, in, r, c + 2);
        MEMCPY_64BYTES(dst, in, r, c + 3);
        MEMCPY_64BYTES(dst, in, r, c + 4);
        MEMCPY_64BYTES(dst, in, r, c + 5);
        MEMCPY_64BYTES(dst, in, r, c + 6);
        MEMCPY_64BYTES(dst, in, r, c + 7);
      }
    }
    return;
  }

  // Were are itereating linearly in the read space, so prefetching in read is
  // not needed (the processor does it for us!)

  for (uint64_t r = 0; r < row_size; r += 1) {
    for (uint64_t c = 0; c < row_size / 8; c+=8) {
      __builtin_prefetch(dst + ((((c + prefetch_lookahead + 0)) * row_size * 8)) + ((r) * 8), prefetch::WRITE, 0);
      __builtin_prefetch(dst + ((((c + prefetch_lookahead + 1)) * row_size * 8)) + ((r) * 8), prefetch::WRITE, 0);
      __builtin_prefetch(dst + ((((c + prefetch_lookahead + 2)) * row_size * 8)) + ((r) * 8), prefetch::WRITE, 0);
      __builtin_prefetch(dst + ((((c + prefetch_lookahead + 3)) * row_size * 8)) + ((r) * 8), prefetch::WRITE, 0);
      __builtin_prefetch(dst + ((((c + prefetch_lookahead + 4)) * row_size * 8)) + ((r) * 8), prefetch::WRITE, 0);
      __builtin_prefetch(dst + ((((c + prefetch_lookahead + 5)) * row_size * 8)) + ((r) * 8), prefetch::WRITE, 0);
      __builtin_prefetch(dst + ((((c + prefetch_lookahead + 6)) * row_size * 8)) + ((r) * 8), prefetch::WRITE, 0);
      __builtin_prefetch(dst + ((((c + prefetch_lookahead + 7)) * row_size * 8)) + ((r) * 8), prefetch::WRITE, 0);
      MEMCPY_64BYTES(dst, in, r, c + 0);
      MEMCPY_64BYTES(dst, in, r, c + 1);
      MEMCPY_64BYTES(dst, in, r, c + 2);
      MEMCPY_64BYTES(dst, in, r, c + 3);
      MEMCPY_64BYTES(dst, in, r, c + 4);
      MEMCPY_64BYTES(dst, in, r, c + 5);
      MEMCPY_64BYTES(dst, in, r, c + 6);
      MEMCPY_64BYTES(dst, in, r, c + 7);

      // dst[((c) * row_size * 8) + ((r) * 8 + 0)] = in[((c) * 8) + ((r) * row_size + 0)];
      // dst[((c) * row_size * 8) + ((r) * 8 + 1)] = in[((c) * 8) + ((r) * row_size + 1)];
      // dst[((c) * row_size * 8) + ((r) * 8 + 2)] = in[((c) * 8) + ((r) * row_size + 2)];
      // dst[((c) * row_size * 8) + ((r) * 8 + 3)] = in[((c) * 8) + ((r) * row_size + 3)];
      // dst[((c) * row_size * 8) + ((r) * 8 + 4)] = in[((c) * 8) + ((r) * row_size + 4)];
      // dst[((c) * row_size * 8) + ((r) * 8 + 5)] = in[((c) * 8) + ((r) * row_size + 5)];
      // dst[((c) * row_size * 8) + ((r) * 8 + 6)] = in[((c) * 8) + ((r) * row_size + 6)];
      // dst[((c) * row_size * 8) + ((r) * 8 + 7)] = in[((c) * 8) + ((r) * row_size + 7)];
    }
  }
}

template<uint64_t row_size>
void __attribute__ ((noinline))
host_to_fpga_transpose_specialized(uint64_t *dst, const uint64_t *in) {
  host_to_fpga_transpose_impl(dst, in, row_size);
}

void __attribute__ ((noinline))
host_to_fpga_transpose_unspecialized(uint64_t *dst, const uint64_t *in, uint64_t row_size) {
  host_to_fpga_transpose_impl(dst, in, row_size);
}

void __attribute__ ((noinline))
host_to_fpga_transpose(uint64_t *dst, const uint64_t *in, uint64_t row_size) {
  switch (row_size) {
  case 4096:
    return host_to_fpga_transpose_specialized<4096>(dst, in);
  }

  return host_to_fpga_transpose_unspecialized(dst, in, row_size);
}

