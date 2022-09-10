#ifndef ZPRIZE_MSM_FPGA_DRIVER_H
#define ZPRIZE_MSM_FPGA_DRIVER_H

#include <iostream>
#include <vector>

#include "bls12_377_g1/bls12_377_g1.h"
#include "bls12_377_g1/rust_types.h"

class ZprizeMsmFpgaDriver {
private:
  std::vector<bls12_377_g1::Xyzt> points;

public:
  ZprizeMsmFpgaDriver(const std::vector<bls12_377_g1::Xyzt> & points) : points(points) {}

  inline uint64_t numPoints () { return points.size(); }

  // Computes msm using the naive double-and-add algorithm (without horner's
  // rule) Useful for debugging
  void naive_msm(g1_projective_t *out, biginteger256_t *scalars);
};

#endif
