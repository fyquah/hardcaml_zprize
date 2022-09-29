#ifndef PIPPENGER
#define PIPPENGER

#include "bls12_377_g1.h"
namespace bls12_377_g1 {
const int NUM_WINDOWS = 21;
inline int NUM_WINDOW_BITS(int window_idx) { return ((window_idx == 20) ? 13 : 12); }
inline int NUM_BUCKETS(int window_idx) { return (1 << (NUM_WINDOW_BITS(window_idx))); }

void triangleSumUpdate(bls12_377_g1::Xyzt &accum,
                       bls12_377_g1::Xyzt &running,
                       const bls12_377_g1::Xyzt &new_point,
                       GeneralUnifiedAddIntoTemps &temps) {
  // running += new_point
  running.generalUnifiedAddInto(new_point, temps);

  // accum += running
  accum.generalUnifiedAddInto(running, temps);
}

// overwrites triangle_sum
void finalSumUpdate(bls12_377_g1::Xyzt &accum,
                    bls12_377_g1::Xyzt &triangle_sum,
                    int index,
                    GeneralUnifiedAddIntoTemps &temps) {
  // 2 ^ (index) * triangle_sum
  for (int i = 0; i < index; i++) {
    triangle_sum.doubleInPlace(temps);
  }

  // accum += 2^(index) * triangle_sum
  accum.generalUnifiedAddInto(triangle_sum, temps);
}

}  // namespace bls12_377_g1

#endif
