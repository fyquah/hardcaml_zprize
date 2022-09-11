#include <benchmark/benchmark.h>
#include "./utils.h"

static void bench_point_arithmetic(benchmark::State &state) {
  bls12_377_g1::init();
  bls12_377_g1::Xyzt a, b;
  a.setToIdentity();
  b.setToIdentity();

  for (auto _ : state) {
    for (int i = 0; i < 100; i++) {
      a.generalUnifiedAddInto(b);
      b.doubleInPlace();
    }
  }
}

BENCHMARK(bench_point_arithmetic)->UseRealTime();
