#ifndef NTT_PREPROCESSING_H
#define NTT_PREPROCESSING_H

#include <stdint.h>

void __attribute__ ((noinline))
ntt_preprocessing(uint64_t *arg_dst, const uint64_t *arg_in, uint64_t row_size);

#endif
