#include <iostream>
#include <vector>

#include "bls12_377_g1/bls12_377_g1.h"
#include "bls12_377_g1/pippenger.h"
#include "bls12_377_g1/rust_types.h"

#define SCALAR_NUM_BITS 253

namespace {

class ZprizeMsmFpgaDriver {
 private:
  std::vector<bls12_377_g1::Xyzt> points;

 public:
  ZprizeMsmFpgaDriver(const std::vector<bls12_377_g1::Xyzt> &points)
      : points(points) {}

  inline uint64_t numPoints() { return points.size(); }

  // Computes msm using the naive double-and-add algorithm (without horner's
  // rule) Useful for debugging
  void naive_msm(g1_projective_t *out, biginteger256_t *scalars) {
    bls12_377_g1::init();
    bls12_377_g1::Xyzt ans;
    ans.setToTwistedEdwardsIdentity();
    for (size_t i = 0; i < numPoints(); i++) {
      std::cout << "POINT " << i << std::endl;
      std::cout << "SCALAR[" << i << "] = ";
      std::cout << scalars[i] << std::endl;
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

  void pippenger_msm(g1_projective_t *out, biginteger256_t *scalars) {
    bls12_377_g1::init();
    bls12_377_g1::Xyzt final_result;
    final_result.setToTwistedEdwardsIdentity();

    bls12_377_g1::Xyzt accum, running;
    bls12_377_g1::GeneralUnifiedAddIntoTemps temps;

    int bit_offset = 0;
    for (int window_idx = 0; window_idx < bls12_377_g1::NUM_WINDOWS;
         window_idx++) {
      const auto CUR_WINDOW_LEN = bls12_377_g1::NUM_WINDOW_BITS(window_idx);
      const auto CUR_NUM_BUCKETS = bls12_377_g1::NUM_BUCKETS(window_idx);

      // compute bucket sums (model fpga)
      std::vector<bls12_377_g1::Xyzt> bucket_sums(CUR_NUM_BUCKETS);
      for (auto &pt : bucket_sums) pt.setToTwistedEdwardsIdentity();
      for (size_t pt_idx = 0; pt_idx < numPoints(); pt_idx++) {
        const uint64_t bucket =
            scalars[pt_idx].getSlice(bit_offset, CUR_WINDOW_LEN);
        assert(bucket < (uint64_t)CUR_NUM_BUCKETS);
        bucket_sums[bucket].generalUnifiedAddInto(points[pt_idx], temps);
      }

      // perform triangle sum
      accum.setToTwistedEdwardsIdentity();
      running.setToTwistedEdwardsIdentity();
      for (int bucket_idx = CUR_NUM_BUCKETS - 1; bucket_idx >= 1;
           bucket_idx--) {
        bls12_377_g1::triangleSumUpdate(accum, running, bucket_sums[bucket_idx],
                                        temps);
      }
      bls12_377_g1::finalSumUpdate(final_result, accum, bit_offset, temps);
      bit_offset += CUR_WINDOW_LEN;
    }

    final_result.extendedTwistedEdwardsToWeierstrass();
    final_result.copy_to_rust_type(*out);
  }
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

}  // namespace

extern "C" ZprizeMsmFpgaDriver *zprize_msm_fpga_init(g1_affine_t *rust_points,
                                                     ssize_t npoints) {
  bls12_377_g1::init();
  std::vector<bls12_377_g1::Xyzt> points(npoints);
  for (ssize_t i = 0; i < npoints; i++) {
    std::cout << rust_points[i] << std::endl;
    points[i].copy_from_rust_type(rust_points[i]);
    points[i].println();
  }
  auto *driver = new ZprizeMsmFpgaDriver(points);
  return driver;
}

extern "C" void zprize_msm_fpga_mult(ZprizeMsmFpgaDriver *context,
                                     g1_projective_t *out, uint64_t batch_size,
                                     biginteger256_t *scalars) {
  // TODO(fyquah): I don't think this works yet ...
  for (uint64_t i = 0; i < batch_size; i++) {
    context->naive_msm(out + i, scalars + (i * context->numPoints()));
  }
}

extern "C" void zprize_msm_pippenger_mult(ZprizeMsmFpgaDriver *context,
                                          g1_projective_t *out,
                                          uint64_t batch_size,
                                          biginteger256_t *scalars) {
  for (uint64_t i = 0; i < batch_size; i++) {
    context->pippenger_msm(out + i, scalars + (i * context->numPoints()));
  }
}
