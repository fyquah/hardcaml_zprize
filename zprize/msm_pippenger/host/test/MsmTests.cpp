#include "bls12_377_g1/bls12_377_g1.h"
#include <gtest/gtest.h>

TEST(PointArithmetic, Add) {
  bls12_377_g1::init();
  bls12_377_g1::Xyzt a, b;
  a.setToIdentity();
  b.setToIdentity();
  a.println();

  for (int i = 0; i < 10; i++) {
    a.generalUnifiedAddInto(b);
    b.doubleInPlace();
    a.doubleInPlace();
    a.println();
  }
}
