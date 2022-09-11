#ifndef ZPRIZE_MSM_FPGA_DRIVER_H
#define ZPRIZE_MSM_FPGA_DRIVER_H

#include <iostream>
#include <vector>

#include "bls12_377_g1/bls12_377_g1.h"
#include "bls12_377_g1/rust_types.h"

#define SCALAR_NUM_BITS 253

class ZprizeMsmFpgaDriver {
 private:
  std::vector<bls12_377_g1::Xyzt> points;

 public:
  ZprizeMsmFpgaDriver(const std::vector<bls12_377_g1::Xyzt> &points) : points(points) {}

  inline uint64_t numPoints() { return points.size(); }

  // Computes msm using the naive double-and-add algorithm (without horner's
  // rule) Useful for debugging
  void naive_msm(g1_projective_t *out, biginteger256_t *scalars) {
    bls12_377_g1::init();
    bls12_377_g1::Xyzt ans;
    ans.setToIdentity();
    for (size_t i = 0; i < numPoints(); i++) {
      std::cout << "POINT " << i << std::endl;
      auto point = points[i];
      for (size_t j = 0; j < SCALAR_NUM_BITS; j++) {
        std::cout << "SCALAR BIT " << j << std::endl;
        if (scalars[i].getBit(j)) {
          std::cout << "ABOUT TO ADD"
                    << "(" << i << "," << j << ")" << std::endl;
          ans.generalUnifiedAddInto(point);
          std::cout << " -> DONE WITH ADD" << std::endl;
        }
        std::cout << "BEGIN DOUBLE" << std::endl;
        point.doubleInPlace();
        std::cout << "FINISH DOUBLE" << std::endl;
      }
    }
    ans.extendedTwistedEdwardsToWeierstrass();
    ans.copy_to_rust_type(*out);
  }
};

#endif
