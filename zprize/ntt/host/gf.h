#ifndef GF_H
#define GF_H

#include <stdint.h>

static const uint64_t MODULUS = 0xFFFFFFFF00000001;
static const uint64_t MASK    = 0xFFFFFFFFFFFFFFFF;
static const uint64_t EPSILON =         0xFFFFFFFF;

class GF {
private:
  uint64_t data;

public:
  GF(uint64_t data) : data(data) {}

  GF(){}

  GF operator+(GF other) {
    uint64_t result;
    bool overflow = __builtin_add_overflow(data, other.data, &result);
    if (overflow || result >= MODULUS)
      return result - MODULUS;
    return GF(result);
  }

  GF operator-(GF other) {
    uint64_t result = data - other.data;
    if (data < other.data) {
      result += MODULUS;
    }
    return GF(result);
  }

  GF operator*(GF other) {
    __uint128_t res = __uint128_t(data) * __uint128_t(other.data);
    uint64_t res_lo = res;
    uint64_t res_hi = (res >> 64);
    uint64_t res_hi_hi = res_hi >> 32;
    uint64_t res_hi_lo = res_hi & EPSILON;
    uint64_t t0;
    bool underflow = __builtin_sub_overflow(res_lo, res_hi_hi, &t0);
    if (underflow) {
      t0 = t0 - EPSILON;
    }
    uint64_t t1 = res_hi_lo * EPSILON;
    uint64_t final_res;
    bool overflow = __builtin_add_overflow(t0, t1, &final_res);
    if (overflow) {
      final_res = final_res + EPSILON;
    }

    if (final_res >= MODULUS) {
      final_res -= MODULUS;
    }

    return final_res;
  }

  uint64_t to_uint64() const {
    return data;
  }
};

static const GF OMEGA[33] = {
	GF(1ull),						// for a domain of 2^0
	GF(18446744069414584320ull),	// for a domain of 2^1
	GF(281474976710656ull),			// for a domain of 2^2
	GF(18446744069397807105ull),	// for a domain of 2^3
	GF(17293822564807737345ull),	// for a domain of 2^4
	GF(70368744161280ull),			// for a domain of 2^5
	GF(549755813888ull),			// for a domain of 2^6
	GF(17870292113338400769ull),	// for a domain of 2^7
	GF(13797081185216407910ull),	// for a domain of 2^8
	GF(1803076106186727246ull),		// for a domain of 2^9
	GF(11353340290879379826ull),	// for a domain of 2^10
	GF(455906449640507599ull),		// for a domain of 2^11
	GF(17492915097719143606ull),	// for a domain of 2^12
	GF(1532612707718625687ull),		// for a domain of 2^13
	GF(16207902636198568418ull),	// for a domain of 2^14
	GF(17776499369601055404ull),	// for a domain of 2^15
	GF(6115771955107415310ull),		// for a domain of 2^16
	GF(12380578893860276750ull),	// for a domain of 2^17
	GF(9306717745644682924ull),		// for a domain of 2^18
	GF(18146160046829613826ull),	// for a domain of 2^19
	GF(3511170319078647661ull),		// for a domain of 2^20
	GF(17654865857378133588ull),	// for a domain of 2^21
	GF(5416168637041100469ull),		// for a domain of 2^22
	GF(16905767614792059275ull),	// for a domain of 2^23
	GF(9713644485405565297ull),		// for a domain of 2^24
	GF(5456943929260765144ull),		// for a domain of 2^25
	GF(17096174751763063430ull),	// for a domain of 2^26
	GF(1213594585890690845ull), 	// for a domain of 2^27
	GF(6414415596519834757ull), 	// for a domain of 2^28
	GF(16116352524544190054ull), 	// for a domain of 2^29
	GF(9123114210336311365ull), 	// for a domain of 2^30
	GF(4614640910117430873ull), 	// for a domain of 2^31
	GF(1753635133440165772ull) 		// for a domain of 2^32
};

#endif  // GF_H
