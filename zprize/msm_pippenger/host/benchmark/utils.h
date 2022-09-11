#ifndef BENCHMARK_UTILS_H
#define BENCHMARK_UTILS_H

#define BENCH(NAME, ...) BENCHMARK_TEMPLATE(bench_##NAME, ##__VA_ARGS__)->UseRealTime()

#endif  // BENCHMARK_UTILS_H
