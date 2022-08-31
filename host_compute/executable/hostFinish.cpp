#include "bls12_377_g1/bls12_377_g1.h"

void triangleSumUpdate(bls12_377_g1::Xyzt &accum, bls12_377_g1::Xyzt &running,
                       const bls12_377_g1::Xyzt &new_point) {
  printf("CALLED triangleSumUpdate\n");
  // running += new_point
  running.addInto(new_point);

  // accum += running
  accum.addInto(running);
}

int main() {
  printf("HOST FINISH START\n");
  bls12_377_g1::init();

  bls12_377_g1::Xyzt accum, running;
  printf("Initialized empty\n");
  bls12_377_g1::Xyzt p;
  printf("Initialized p\n");
  triangleSumUpdate(accum, running, p);
  printf("done with triangle sum\n");

  accum.println("accum");
  running.println("running");
  p.println("p");

  return 0;
}
