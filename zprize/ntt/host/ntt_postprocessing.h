#ifndef NTT_POSTPROCESSING_H
#define NTT_POSTPROCESSING_H

#include <stdint.h>

void __attribute__ ((noinline))
ntt_postprocessing(
    uint64_t *arg_dst,
    const uint64_t *arg_in,
    uint64_t row_size,
    uint64_t logblocks
    );

#endif

