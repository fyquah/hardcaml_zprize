#include "bls12_377_g1/rust_types.h"

#include "zprize_msm_fpga_driver.h"

#define SCALAR_NUM_BITS 253

void ZprizeMsmFpgaDriver::naive_msm(g1_projective_t *out,
                                    biginteger256_t *scalars) {
  bls12_377_g1::Xyzt ans;
  ans.setToIdentity();
  for (size_t i = 0; i < numPoints(); i++) {
    auto point = points[i];
    for (size_t j = 0; j < SCALAR_NUM_BITS; j++) {
      if (scalars[i].getBit(j)) {
        ans.generalUnifiedAddInto(point);
      }
      point.doubleInPlace();
    }
  }
  ans.extendedTwistedEdwardsToWeierstrass();
  ans.copy_to_rust_type(*out);
}
