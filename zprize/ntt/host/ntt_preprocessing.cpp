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

#define MEMCPY_64BYTES(dst, src) \
     memcpy(dst, src, sizeof(uint64_t) * 8); 

#define LOGCORES 3
#define NUM_CORES (1 << LOGCORES)

static void inline __attribute__ ((always_inline))
ntt_preprocessing_impl(uint64_t *dst, const uint64_t *in, uint64_t row_size){
  // [dst] is a 8192-byte align addresss by design, since it points to the special
  // memory region used by XRT for buffers
  dst = (uint64_t*) __builtin_assume_aligned((void*) dst, 64);

  for (uint64_t r = 0; r < row_size; r += 1) {
    for (uint64_t c = 0; c < row_size >> LOGCORES; c++) {
      MEMCPY_64BYTES(&dst[(c * row_size * NUM_CORES) + (r * NUM_CORES)], &in[r * row_size + (c * NUM_CORES)]);
    }
  }
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
