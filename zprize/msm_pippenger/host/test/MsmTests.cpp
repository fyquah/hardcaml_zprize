#include "bls12_377_g1/bls12_377_g1.h"
#include <gtest/gtest.h>

TEST(PointArithmetic, Add) {
  bls12_377_g1::init();
  // generate random point
  uint64_t RAND_WORDS[3][bls12_377_g1::NUM_64B_WORDS];
  for (int i = 0; i < 3; i++) {
    for (int j = 0; j < bls12_377_g1::NUM_64B_WORDS; j++) {
      RAND_WORDS[i][j] = std::rand() % (1 << 29);
    }
  }
  bls12_377_g1::Xyzt a(RAND_WORDS[0], RAND_WORDS[1], RAND_WORDS[2], RAND_WORDS[2]);
  a.t.set_mul(a.x, a.y);
  a.t.set_mul(a.t, a.z);

  bls12_377_g1::Xyzt b;
  b.setToTwistedEdwardsIdentity();
  a.println();

  for (int i = 0; i < 10; i++) {
    // should never change
    a.generalUnifiedAddInto(b);
    bls12_377_g1::Xyzt temp(a);
    temp.twistedEdwardsExtendedToAffine();
    temp.println();
  }
}
