#include <assert.h>
#include <stdint.h>

#include "gf.h"
#include "reference.h"

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

static uint64_t __attribute__ ((noinline))
slow_modulo_mult(uint64_t a, uint64_t b)
{
  __uint128_t mult_result = __uint128_t(a) * __uint128_t(b);
  return (mult_result % MODULUS);
}

static uint64_t __attribute__ ((noinline))
slow_modulo_pow(uint64_t base, uint64_t pow) {
  if (pow == 0)
    return 1;

  if (pow == 1)
    return base;

  uint64_t tmp = slow_modulo_pow(base, pow/2);

  if (pow % 2 == 0) {
    return slow_modulo_mult(tmp, tmp);
  }

  return slow_modulo_mult(slow_modulo_mult(tmp, tmp), base);
}

static void
slow_apply_twiddle(uint64_t *data, uint64_t log_row_size)
{
  uint64_t w = OMEGA[2*log_row_size].to_uint64();
  uint64_t row_size = 1 << log_row_size;

  for (uint64_t i = 0; i < row_size; i++)
    for (uint64_t j = 0; j < row_size; j++)
      data[(i * row_size) + j] = slow_modulo_mult(
          data[(i * row_size) + j],
          slow_modulo_pow(w, i * j));
}

static void
ntt_reference_reverse(
    uint64_t* dst,
    const uint64_t* src,
    const uint64_t log_row_size)
{
  uint64_t row_size = 1 << log_row_size;
  uint64_t num_elements = row_size * row_size;
  memcpy(dst, src, sizeof(uint64_t) * num_elements);

  // phase 1, read transpose, write transpose
  transpose(dst, row_size);
  for (size_t i = 0; i < row_size; i++) {
    do_stuff(dst + (i * row_size), row_size);
  }
  transpose(dst, row_size);

  // phase 1.5, apply twiddling on host
  slow_apply_twiddle(dst, log_row_size);

  // phase 2, read linear, write transpose
  for (size_t i = 0; i < row_size; i++) {
    do_stuff(dst + (i * row_size), row_size);
  }
  transpose(dst, row_size);
}


static void
ntt_reference_inplace(uint64_t *data, uint64_t logn) {
  uint64_t n = 1 << logn;

  // super slow bitreverse implementation
  auto bitreverse = [=](uint64_t x){
    uint64_t acc = 0;
    for (uint64_t i = 0; i < logn; i++) {
      acc = acc << 1;
      acc |= (x >> i) & 1;
    }
    return acc;
  };

  GF tmp;
  for (uint64_t k = 0; k < n; k++) {
    uint64_t rk = bitreverse(k);
    if (k < rk) {
      std::swap(data[k], data[rk]);
    }
  }

  uint64_t m = 1;
  for (uint64_t i = 1; i <= logn; i++) {
    // w_m is 2^i-th root of unity
    GF w_m = OMEGA[i];

    uint64_t k = 0;
    while (k < n) {
      // w = w_m^j at the start of every loop iteration
      GF w(1);

      for (uint64_t j = 0; j < m; j++) {
        GF t = GF(data[k + j + m]);
        t = t * w;

        GF tmp = GF(data[k + j]);
        tmp = tmp - t;

        data[k + j + m] = tmp.to_uint64();
        data[k + j] = (GF(data[k + j]) + t).to_uint64();

        w = w * w_m;
      }

      k += 2 * m;
    }

    m *= 2;
  }
}

static void
ntt_reference_actual(uint64_t *dst, const uint64_t *src, uint64_t logn) {
  memcpy(dst, src, sizeof(uint64_t) * (1ull << logn));
  ntt_reference_inplace(dst, logn);
}

void
ntt_reference(uint64_t* dst, const uint64_t* src, NttFpgaDriverArg driver_arg)
{
  if (driver_arg.core_type == CoreType::REVERSE)
    return ntt_reference_reverse(dst, src, driver_arg.log_row_size);

  return ntt_reference_actual(dst, src, 2 * driver_arg.log_row_size);
}
