#ifndef SETUP_STREAM_H
#define SETUP_STREAM_H

#include <stdint.h>
#include <string.h>

uint64_t msm_setup_precomputed_points_stream(uint64_t address,
                                             uint64_t num_points,
                                             const uint64_t *x,
                                             const uint64_t *y,
                                             uint64_t *dst_base);

uint64_t msm_setup_scalars_stream(uint64_t num_scalars,
                                  uint64_t num_scalars_per_batch,
                                  uint64_t num_bits,
                                  const uint8_t *scalar_ptr_base,
                                  uint16_t *dst);

#endif
