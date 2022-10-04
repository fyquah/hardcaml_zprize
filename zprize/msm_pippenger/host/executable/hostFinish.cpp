#include "bls12_377_g1/bls12_377_g1.h"
#include "bls12_377_g1/pippenger.h"

// This is an example structure of the final summation, need to incorporate it into openCL.
int main() {
  bls12_377_g1::init();

  bls12_377_g1::Xyzt final_result;
  bls12_377_g1::Xyzt accum, running;
  bls12_377_g1::GeneralUnifiedAddIntoTemps temps;

  int bit_offset = 0;
  for (int window_idx = 0; window_idx < bls12_377_g1::NUM_WINDOWS; window_idx++) {
    accum.setToTwistedEdwardsIdentity();
    running.setToTwistedEdwardsIdentity();
    for (int bucket_idx = bls12_377_g1::NUM_BUCKETS(window_idx) - 1; bucket_idx >= 0;
         bucket_idx--) {
      bls12_377_g1::Xyzt p;
      // TODO: receive [p] somehow!
      p.postComputeFPGA();
      bls12_377_g1::triangleSumUpdate(accum, running, p, temps);
    }
    bls12_377_g1::finalSumUpdate(final_result, accum, bit_offset, temps);
    bit_offset += bls12_377_g1::NUM_WINDOW_BITS(window_idx);
  }

  final_result.extendedTwistedEdwardsToWeierstrass();
  final_result.println();
}
