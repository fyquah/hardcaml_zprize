#ifndef RUST_TYPES_H
#define RUST_TYPES_H

#include <stdint.h>

#include <iostream>

struct biginteger256_t {
  uint64_t data[4];


  biginteger256_t& operator=(const biginteger256_t&) = delete;
biginteger256_t(const biginteger256_t &other) {
printf("copying bigint256\n");
for(int i =0; i <4; i++) data[i] = other.data[i];
//memcpy(data, other.data, sizeof(data));

}
  inline uint64_t &operator[](uint64_t i) { return data[i]; }

  inline uint64_t getBit(uint64_t i) const {
    // std::cout << "getBit" << std::endl;
    return (data[i / 64] >> (i % 64)) & 1;
  }

  inline void copy_to_fpga_buffer(void *b) {
    // memcpy(b, data, sizeof(data));
    uint64_t *b64 = (uint64_t *)b;
    for (int i = 0; i < 4; i++) {
      b64[i] = data[i];
    }
  }

  inline uint64_t getSlice(int start, int len) {
    if (len >= 64) {
      throw "Invalid";
    }
    int end = start + len - 1;

    int start_word = start / 64;
    int start_offset = start % 64;
    int end_word = end / 64;
    int end_offset = end % 64;

    uint64_t res = 0ULL;
    int cur_offset = 0;
    for (int word = start_word; word <= end_word; word++) {
      uint64_t cur_word = data[word];
      int cur_len = 64;
      if (word == end_word) {
        cur_len = end_offset + 1;
      }
      if (word == start_word) {
        cur_word >>= start_offset;
        cur_len -= start_offset;
      }
      cur_word &= ((1ULL << cur_len) - 1ULL);
      res |= ((cur_word) << cur_offset);
      cur_offset += cur_len;
    }
    return res;
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

namespace {
std::ostream __attribute__((unused)) &operator<<(std::ostream &os,
                                                 const biginteger256_t &point) {
  os << "(";
  for (int i = 0; i < 4; i++) {
    if (i != 0) {
      os << ", ";
    }
    os << point.data[i];
  }
  return os << ")";
}
}  // namespace

#endif
