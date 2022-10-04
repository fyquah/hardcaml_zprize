#ifndef PIPPENGER
#define PIPPENGER

#include "bls12_377_g1.h"
namespace bls12_377_g1 {
// exactly mirrors [hardcaml/src/config_utils.ml]
const int NUM_WINDOWS = 20;
const int LOWER_WINDOW_SIZE = SCALAR_NUM_BITS / NUM_WINDOWS;
const int NUM_HIGHER_WINDOWS =
    SCALAR_NUM_BITS - (LOWER_WINDOW_SIZE * NUM_WINDOWS);

inline int NUM_WINDOW_BITS(int window_idx) {
  return ((window_idx < NUM_HIGHER_WINDOWS) ? LOWER_WINDOW_SIZE + 1
                                            : LOWER_WINDOW_SIZE);
}
inline int NUM_BUCKETS(int window_idx) {
  // -1 trick doesn't apply to last window
  if (window_idx == NUM_WINDOWS - 1) {
    return (1 << (NUM_WINDOW_BITS(window_idx))) - 1;
  }

  return 1 << (NUM_WINDOW_BITS(window_idx) - 1);
}

void triangleSumUpdate(bls12_377_g1::Xyzt &accum, bls12_377_g1::Xyzt &running,
                       const bls12_377_g1::Xyzt &new_point,
                       GeneralUnifiedAddIntoTemps &temps) {
  // running += new_point
  running.generalUnifiedAddInto(new_point, temps);

  // accum += running
  accum.generalUnifiedAddInto(running, temps);
}

// overwrites triangle_sum
void finalSumUpdate(bls12_377_g1::Xyzt &accum, bls12_377_g1::Xyzt &triangle_sum,
                    int index, GeneralUnifiedAddIntoTemps &temps) {
  // 2 ^ (index) * triangle_sum
  for (int i = 0; i < index; i++) {
    triangle_sum.doubleInPlace(temps);
  }

  // accum += 2^(index) * triangle_sum
  accum.generalUnifiedAddInto(triangle_sum, temps);
}

}  // namespace bls12_377_g1

#endif
