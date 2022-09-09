#include "bls12_377_g1/bls12_377_g1.h"

static const int NUM_POINTS = (1 << 26);
int main() {
  bls12_377_g1::init();

  for (int i = 0; i < NUM_POINTS; i++) {
    bls12_377_g1::Xyzt p;
    // TODO: receive point p from somewhere
    p.affineWeierstrassToExtendedTwistedEdwards();
    p.preComputeFPGA();
    // TODO: send point p to the FPGA
  }
}
