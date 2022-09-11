#ifndef RUST_TYPES_H
#define RUST_TYPES_H

#include <iostream>
#include <stdint.h>

struct biginteger256_t {
  uint64_t data[4];

  inline uint64_t &operator[](uint64_t i) { return data[i]; }

  inline uint64_t getBit(uint64_t i) {
    std::cout << "getBit" << std::endl;
    return (data[i / 64] >> (i % 64)) & 1;
  }
};

struct biginteger384_t {
  uint64_t data[6];

  inline uint64_t &operator[](uint64_t i) { return data[i]; }

  inline uint64_t getBit(uint64_t i) { return (data[i / 64] >> (i % 64)) & 1; }
};

struct g1_affine_t {
  biginteger384_t x;
  biginteger384_t y;
  bool infinity;
};

struct g1_projective_t {
  biginteger384_t x;
  biginteger384_t y;
  biginteger384_t z;
};

#endif
