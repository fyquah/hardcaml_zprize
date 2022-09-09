#ifndef PIPPENGER
#define PIPPENGER

#include "bls12_377_g1.h"
namespace bls12_377_g1 {

void triangleSumUpdate(bls12_377_g1::Xyzt &accum, bls12_377_g1::Xyzt &running,
                       const bls12_377_g1::Xyzt &new_point) {
  printf("CALLED triangleSumUpdate\n");

  // running += new_point
  running.generalUnifiedAddInto(new_point);

  // accum += running
  accum.generalUnifiedAddInto(running);
}

// overwrites triangle_sum
void finalSumUpdate(bls12_377_g1::Xyzt &accum, bls12_377_g1::Xyzt &triangle_sum, int index) {
  // 2 ^ (index) * triangle_sum
  for (int i = 0; i < index; i++) {
    triangle_sum.doubleInPlace();
  }

  // accum += 2^(index) * triangle_sum
  accum.generalUnifiedAddInto(triangle_sum);
}
}  // namespace bls12_377_g1

#endif
