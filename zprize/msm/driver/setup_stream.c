#include "setup_stream.h"

#define NUM_BYTES_PER_377 48
#define NUM_WORDS_PER_377 (48 / 8)

uint64_t msm_setup_precomputed_points_stream(uint64_t address,
                                             uint64_t num_points,
                                             const uint64_t *x,
                                             const uint64_t *y,
                                             uint64_t *dst_base) {
  uint64_t *dst = dst_base;
  for (uint64_t i = 0; i < 4; i++) {
    dst[i] = 0;
  }
  memset(dst, 0, 32);
  *dst = address;
  dst += 4;
  for (uint64_t i = 0; i < num_points; i++) {
    memcpy(dst, x, NUM_BYTES_PER_377);
    dst += NUM_WORDS_PER_377;

    memcpy(dst, y, NUM_BYTES_PER_377);
    dst += NUM_WORDS_PER_377;

    x += NUM_WORDS_PER_377;
    y += NUM_WORDS_PER_377;
  }

  return (dst - dst_base) * 8;
}

uint64_t msm_setup_scalars_stream(uint64_t num_scalars,
                                  uint64_t num_scalars_per_batch,
                                  uint64_t num_bits,
                                  const uint8_t *scalar_ptr_base,
                                  uint16_t *dst) {
  uint16_t *dst_base = dst;

  for (int64_t bit_position = num_bits - 1; bit_position >= 0; bit_position--) {
    const uint8_t *scalar_ptr = scalar_ptr_base + (bit_position / 8);
    const uint64_t bit_offset = bit_position % 8;
    const uint8_t mask = 1ull << bit_offset;
    uint64_t position_in_batch = 0;
    uint16_t running_word = 0;
    uint16_t batch_index = 0;

    for (uint64_t i = 0; i < num_scalars; i++) {
      running_word |= (((uint64_t)(*scalar_ptr & mask)) >> bit_offset)
                      << position_in_batch;
      scalar_ptr += NUM_BYTES_PER_377;

      position_in_batch += 1;
      if (position_in_batch == num_scalars_per_batch) {
        *(dst++) = (batch_index << num_scalars_per_batch) | running_word;
        batch_index++;
        position_in_batch = 0;
        running_word = 0;
      }
    }

    if (position_in_batch != 0)
      *(dst++) = (batch_index << num_scalars_per_batch) | running_word;
  }

  return (dst - dst_base) * 2;
}
