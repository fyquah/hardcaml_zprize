#include <iostream>

#include "bls12_377_g1/bls12_377_g1.h"

namespace {

struct biginteger384_t {
  uint64_t data[6];

  uint64_t &operator[](size_t i) { return data[i]; }
};

struct g1_affine_t {
  biginteger384_t x;
  biginteger384_t y;
  bool infinity;
};

struct g1_projective_t {
  biginteger384_t x;
  biginteger384_t y;
  biginteger384_t z;
};

std::ostream &operator<<(std::ostream &os, const biginteger384_t &point) {
  os << "(";
  for (int i = 0; i < 6; i++) {
    if (i != 0) {
      os << ", ";
    }
    os << point.data[i];
  }
  return os << ")";
}

std::ostream &operator<<(std::ostream &os, const g1_affine_t &point) {
  return os << "{ x: " << point.x << " , y: " << point.y
            << ", infinity: " << (point.infinity ? "true" : "false") << " }";
}

class ZprizeMsmFpgaContext {
  uint64_t x;
};

} // namespace

extern "C" ZprizeMsmFpgaContext *zprize_msm_fpga_init(g1_affine_t *points,
                                                      ssize_t npoints) {
  auto *context = new ZprizeMsmFpgaContext();
  return context;
}

extern "C" void zprize_msm_fpga_mult(ZprizeMsmFpgaContext *context,
                                     g1_projective_t *out, uint64_t batch_size,
                                     uint64_t npoints) {
  memset(out, 0, batch_size * sizeof(g1_projective_t));
  for (int i = 0; i < batch_size; i++) {
    out[i].x[0] = i;
  }
}
