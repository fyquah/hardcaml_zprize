#include "bls12_377_g1/bls12_377_g1.h"
#include "bls12_377_g1/pippenger.h"

// This is an example structure of the final summation, need to incorporate it into openCL.
static const int NUM_WINDOWS = 21;
static inline int NUM_BITS(int window_idx) { return ((window_idx == 0) ? 13 : 12); }
static inline int NUM_BUCKETS(int window_idx) { return (1 << (NUM_BITS(window_idx))); }

int main() {
  bls12_377_g1::init();

  bls12_377_g1::Xyzt final_result;
  bls12_377_g1::Xyzt accum, running;
  int bit_offset = 0;
  for (int window_idx = 0; window_idx < NUM_WINDOWS; window_idx++) {
    accum.setToIdentity();
    running.setToIdentity();
    for (int bucket_idx = NUM_BUCKETS(window_idx) - 1; bucket_idx >= 0; bucket_idx--) {
      bls12_377_g1::Xyzt p;
      // TODO: receive [p] somehow!
      p.postComputeFPGA();
      bls12_377_g1::triangleSumUpdate(accum, running, p);
    }
    bls12_377_g1::finalSumUpdate(final_result, accum, bit_offset);
    bit_offset += NUM_BITS(window_idx);
  }

  final_result.extendedTwistedEdwardsToWeierstrass();
  final_result.println();
}
