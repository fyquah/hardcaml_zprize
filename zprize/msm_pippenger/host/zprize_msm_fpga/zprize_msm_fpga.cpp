#include <iostream>
#include <vector>

#include "bls12_377_g1/bls12_377_g1.h"
#include "bls12_377_g1/rust_types.h"

#include "zprize_msm_fpga_driver.h"

namespace {

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

} // namespace

extern "C" ZprizeMsmFpgaDriver *zprize_msm_fpga_init(g1_affine_t *rust_points,
                                                      ssize_t npoints) {

  std::vector<bls12_377_g1::Xyzt> points(npoints);
  for (ssize_t i = 0; i < npoints; i++) {
    points[i].copy_from_rust_type(rust_points[i]);
  }
  auto *driver = new ZprizeMsmFpgaDriver(points);
  return driver;
}

extern "C" void zprize_msm_fpga_mult(ZprizeMsmFpgaDriver *context,
                                     g1_projective_t *out,
                                     uint64_t batch_size,
                                     biginteger256_t *scalars) {
  for (uint64_t i = 0; i < batch_size; i++) {
    context->naive_msm(out + i, scalars + (i * context->numPoints()));
  }
}
