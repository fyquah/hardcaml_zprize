#ifndef LOG_TIME_H
#define LOG_TIME_H

#include <chrono>
#include <iostream>
using namespace std::chrono;

class LogTimeTaken {
 private:
  const char *descr;
  std::chrono::time_point<std::chrono::steady_clock> start;

 public:
  LogTimeTaken(const char *descr)
      : descr(descr), start(std::chrono::steady_clock::now()) {}

  ~LogTimeTaken() {
    auto end = std::chrono::steady_clock::now();
    std::chrono::duration<double> elapsed_seconds = end - start;
    std::cout << "[" << descr << "] " << elapsed_seconds.count() << "s\n";
  }
};

template <typename F>
static auto bench(const char *descr, F f) {
  LogTimeTaken log_time_taken(descr);
  return f();
}

#endif
