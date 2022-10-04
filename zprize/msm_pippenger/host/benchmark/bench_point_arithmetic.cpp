#include <random>
#include <benchmark/benchmark.h>
#include "./utils.h"
#include "bls12_377_g1/bls12_377_g1.h"

static void bench_point_arithmetic(benchmark::State &state) {
  bls12_377_g1::init();
  bls12_377_g1::Xyzt a, b;
  a.setToTwistedEdwardsIdentity();
  b.setToTwistedEdwardsIdentity();

  for (auto _ : state) {
    for (int i = 0; i < 100; i++) {
      a.generalUnifiedAddInto(b);
      b.doubleInPlace();
    }
  }
}

static void bench_twisted_edwards_to_weierstrass(benchmark::State &state) {
  bls12_377_g1::init();
  uint64_t RAND_WORDS[4][bls12_377_g1::NUM_64B_WORDS];
  for (int i = 0; i < 4; i++) {
    for (int j = 0; j < bls12_377_g1::NUM_64B_WORDS; j++) {
      RAND_WORDS[i][j] = std::rand() % (1 << 29);
    }
  }
  bls12_377_g1::Xyzt a(RAND_WORDS[0], RAND_WORDS[1], RAND_WORDS[2], RAND_WORDS[3]);

  for (auto _ : state) {
    a.extendedTwistedEdwardsToWeierstrass();
  }
}

BENCHMARK(bench_point_arithmetic)->UseRealTime();
BENCHMARK(bench_twisted_edwards_to_weierstrass)->UseRealTime();
