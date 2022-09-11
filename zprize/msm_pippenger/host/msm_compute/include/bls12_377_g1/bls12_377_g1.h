
#ifndef BLS12_377_G1
#define BLS12_377_G1

#include <iostream>
#include <stdio.h>
#include <gmp.h>
#include <cstdint>
#include <cassert>
#include <cstring>

#include "rust_types.h"

namespace bls12_377_g1 {

const int NUM_BITS = 377;
const int NUM_32B_WORDS = (NUM_BITS + 31) / 32;

const uint32_t ZERO_WORDS[NUM_32B_WORDS] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
const uint32_t ONE_WORDS[NUM_32B_WORDS] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1};
const uint32_t TWO_WORDS[NUM_32B_WORDS] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2};
const uint32_t FOUR_WORDS[NUM_32B_WORDS] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4};

// parameters
const uint32_t Q_WORDS[NUM_32B_WORDS] = {28195398,  398790890, 3325756864, 1822509371,
                                         438491635, 16061327,  519266863,  3121170432,
                                         386620740, 805306368, 2231943168, 1};
const uint32_t S_WORDS[NUM_32B_WORDS] = {27084756,   364248928,  2826603387, 3132251830,
                                         1115542924, 2744010499, 251469411,  279739379,
                                         127017407,  726390572,  1359407591, 265052502};
const uint32_t TWISTED_SCALE_WORDS[NUM_32B_WORDS] = {
    3554049,    3749814336, 1585848431, 751109329,  1198861201, 3060062493,
    1283653330, 395325160,  1949648005, 2205131724, 3124426041, 3245573499};
const uint32_t A_WORDS[NUM_32B_WORDS] = {28195398,  398790890, 3325756864, 1822509371,
                                         438491635, 16061327,  519266863,  3121170432,
                                         386620740, 805306368, 2231943168, 0};
const uint32_t D_WORDS[NUM_32B_WORDS] = {14867693,   4279254641, 1630882440, 359549696,
                                         4268139814, 2686680318, 1600664727, 3383736163,
                                         1566348039, 4153284110, 351450836,  3180630020};
const uint32_t K_WORDS[NUM_32B_WORDS] = {1539989,    3864751095, 4230975311, 3191557318,
                                         3802820698, 1062332013, 2682062591, 3646301894,
                                         2746075339, 3206294555, 2765925801, 2066292743};
const uint32_t C_A_WORDS[NUM_32B_WORDS] = {3331926,    103625886, 1497460430, 365739918,
                                           2263813427, 406087076, 803392357,  4229325863,
                                           778809999,  236747388, 2617606730, 3499809793};
const uint32_t C_B_WORDS[NUM_32B_WORDS] = {27084756,   364248928,  2826603387, 3132251830,
                                           1115542924, 2744010499, 251469411,  279739379,
                                           127017407,  726390572,  1359407591, 265052502};

static void init_empty(mpz_t v) {
  mpz_init(v);
  // mpz_init2(v, 384 + mp_bits_per_limb);  // https://gmplib.org/manual/Initializing-Integers
}
static const int WORDS_MOST_SIGNIFICANT = 1;
static const int WORDS_LEAST_SIGNIFICANT = -1;
static const int BYTES_MOST_SIGNIFICANT = 1;
static const int BYTES_LEAST_SIGNIFICANT = -1;
static void set_words(mpz_t v, const uint32_t words[], bool words_most_sig = true) {
  if (words_most_sig) {
    mpz_import(v, NUM_32B_WORDS, WORDS_MOST_SIGNIFICANT, sizeof(uint32_t), BYTES_LEAST_SIGNIFICANT,
               0, words);
  } else {
    mpz_import(v, 6, WORDS_LEAST_SIGNIFICANT, sizeof(uint64_t), BYTES_LEAST_SIGNIFICANT, 0,
               (uint64_t *)words);
  }
}

mpz_t q;
static bool initialized = false;
void init() {
  if (initialized) {
#ifndef NDEBUG
    printf("Already initialized!");
#endif
  } else {
    initialized = true;
    init_empty(q);
    set_words(q, Q_WORDS);
  }
}

class GFq {
 public:
  mpz_t v;

  void set(const uint32_t words[], bool words_most_sig = true) {
    set_words(v, words, words_most_sig);
  }
  void set(const GFq &other) { mpz_set(v, other.v); }
  // assumes [NUM_32B_WORDS] words
  explicit GFq(const uint32_t words[]) {
    init_empty(v);
    set(words);
  }

  // convenience wrappers for constants
  explicit GFq(const char str[]) {
    init_empty(v);
    mpz_set_str(v, str, 0);
    assert(mpz_sizeinbase(v, 2) <= NUM_BITS);
  }
  explicit GFq(long int val) {
    init_empty(v);
    mpz_set_si(v, val);
  }

  // does not set a value
  GFq() { init_empty(v); }

  ~GFq() { /*mpz_clear(v);*/
  }

  // arithmetic
  void divBy2() {
    bool even = mpz_divisible_2exp_p(v, 1);
    if (!even) {
      mpz_add(v, v, q);  // add q
    }
    mpz_fdiv_q_2exp(v, v, 1);
  }
  void divBy4() {
    // can be optimized by going directly to mod 4
    divBy2();
    divBy2();
  }

  void reduce() { mpz_mod(v, v, q); }
  void set_add(const GFq &a, const GFq &b) {
    mpz_add(v, a.v, b.v);
    reduce();
  }
  void set_sub(const GFq &a, const GFq &b) {
    mpz_sub(v, a.v, b.v);
    reduce();
  }
  void set_mul(const GFq &a, const GFq &b) {
    mpz_mul(v, a.v, b.v);
    reduce();
  }
  void set_div(const GFq &a, const GFq &b) {
    mpz_t b_inverse;
    init_empty(b_inverse);
    mpz_invert(b_inverse, b.v, q);
    mpz_mul(v, a.v, b_inverse);
    reduce();
  }

  void copy_to_rust_type(biginteger384_t &b) {
    mpz_export(
        /* rop */ (void *)b.data,
        /* countp */ nullptr,
        /* order */ WORDS_LEAST_SIGNIFICANT,
        /* size */ sizeof(uint64_t),
        /* endian */ BYTES_LEAST_SIGNIFICANT,
        /* nails */ 0,
        /* op */ v);
  }

  // debugging
  void print() { gmp_printf("%Zd", v); }

  void dumpToWords(const char *str) {
    uint64_t words[NUM_32B_WORDS];
    size_t countp;
    // assert(sizeof(words) == NUM_32B_WORDS * 4);
    memset(words, 0, sizeof(words));

    mpz_export(words, &countp, WORDS_LEAST_SIGNIFICANT, sizeof(uint64_t), BYTES_LEAST_SIGNIFICANT,
               0, v);
    assert(countp <= NUM_32B_WORDS);
    printf("const uint64_t %s_WORDS[NUM_32B_WORDS] = {", str);
    for (int i = 0; i < NUM_32B_WORDS; i++) {
      if (i == 0) {
        printf("%lu", words[i]);
      } else {
        printf(", %lu", words[i]);
      }
    }
    printf("};\n");
  }
};

// parameter constants
const GFq twisted_scale(TWISTED_SCALE_WORDS);
const GFq one(1);
const GFq two(2);
const GFq three(3);
const GFq four(4);
const GFq k(K_WORDS);

struct WeierstrassParams {
  GFq a, b, s, alpha;
  WeierstrassParams(const uint32_t a_words[], const uint32_t b_words[], const uint32_t s_words[],
                    const uint32_t alpha_words[])
      : a(a_words), b(b_words), s(s_words), alpha(alpha_words) {}
};
// a = -1 in twisted edwards, so A_WORDS is -1
const WeierstrassParams weierstrass_params(ZERO_WORDS, ONE_WORDS, S_WORDS, A_WORDS);

struct MontgomeryParams {
  GFq c_A, c_B;
  MontgomeryParams(const uint32_t c_A_words[], const uint32_t c_B_words[])
      : c_A(c_A_words), c_B(c_B_words) {}
};
const MontgomeryParams montgomery_params(C_A_WORDS, C_B_WORDS);

struct TwistedEdwardsParams {
  GFq a, d;
  TwistedEdwardsParams(const uint32_t a_words[], const uint32_t d_words[])
      : a(a_words), d(d_words) {}
};

const TwistedEdwardsParams twisted_edwards_params(A_WORDS, D_WORDS);
class Xyzt {
 public:
  GFq x, y, z, t;

  Xyzt(const uint32_t words_x[], const uint32_t words_y[], const uint32_t words_z[],
       const uint32_t words_t[])
      : x(words_x), y(words_y), z(words_z), t(words_t) {}

  Xyzt() : Xyzt(ZERO_WORDS, ONE_WORDS, ONE_WORDS, ZERO_WORDS) {}

  void setToIdentity() {
    x.set(ZERO_WORDS);
    y.set(ONE_WORDS);
    z.set(ONE_WORDS);
    t.set(ZERO_WORDS);
  }

  // conversions
  void twistedEdwardsAffineToExtended() {
    z.set(ONE_WORDS);
    t.set_mul(x, y);
  }
  void twistedEdwardsExtendedToAffine() {
    x.set_div(x, z);
    y.set_div(y, z);
    z.set(ONE_WORDS);
    t.set(ZERO_WORDS);
  }
  void affineWeierstrassToMontgomery() {
    x.set_sub(x, weierstrass_params.alpha);
    x.set_mul(x, weierstrass_params.s);
    y.set_mul(weierstrass_params.s, y);
  }
  void affineMontgomeryToWeierstrass() {
    GFq temp;
    temp.set_mul(three, montgomery_params.c_B);
    temp.set_add(montgomery_params.c_A, temp);
    x.set_div(x, montgomery_params.c_B);
    x.set_add(x, temp);
    y.set_div(y, montgomery_params.c_B);
  }

  void affineMontgomeryToTwistedEdwards() {
    GFq temp1, temp2, temp3;
    temp3.set_mul(twisted_scale, x);
    temp1.set_sub(x, one);
    temp2.set_add(x, one);
    x.set_div(temp3, y);
    y.set_div(temp1, temp2);
  }
  void affineTwistedEdwardsToMontgomery() {
    GFq temp1, temp2;
    temp1.set_add(one, y);
    temp2.set_sub(one, y);
    temp1.set_div(temp1, temp2);
    temp2.set_div(twisted_scale, x);
    x.set(temp1);
    y.set_mul(temp1, temp2);
  }

  // preprocess function
  void affineWeierstrassToExtendedTwistedEdwards() {
    affineWeierstrassToMontgomery();
    affineMontgomeryToTwistedEdwards();
    twistedEdwardsAffineToExtended();
  }
  // postprocess function
  void extendedTwistedEdwardsToWeierstrass() {
    twistedEdwardsExtendedToAffine();
    affineTwistedEdwardsToMontgomery();
    affineMontgomeryToWeierstrass();
  }

  void generalUnifiedAddInto(const Xyzt &other) {
    // https://hyperelliptic.org/EFD/g1p/auto-twisted-extended-1.html#addition-add-2008-hwcd-3
    GFq A, B, C, D, E, F, G, H;

    GFq temp1, temp2;
    // A
    temp1.set_sub(y, x);
    temp2.set_sub(other.y, other.x);
    A.set_mul(temp1, temp2);

    // B
    temp1.set_add(y, x);
    temp2.set_add(other.y, other.x);
    B.set_mul(temp1, temp2);

    // C - https://gmplib.org/manual/Efficiency (better to multiply out-of-place)
    temp1.set_mul(t, k);
    C.set_mul(temp1, other.t);

    // D
    temp1.set_mul(z, two);
    D.set_mul(temp1, other.z);

    E.set_sub(B, A);
    F.set_sub(D, C);
    G.set_add(D, C);
    H.set_add(B, A);

    x.set_mul(E, F);
    y.set_mul(G, H);
    t.set_mul(E, H);
    z.set_mul(F, G);
  }

  void doubleInPlace() { generalUnifiedAddInto(*this); }

  void postComputeFPGA() {
    x.divBy2();
    y.divBy2();
    z.divBy4();
  }
  void preComputeFPGA() {
    // (x, y, z, t) -> ((y-x)/2,(y+x)/2,4d*t)
    GFq temp;
    temp.set_sub(y, x);
    temp.set_div(temp, two);
    y.set_add(y, x);
    y.set_div(y, two);
    x.set(temp);

    temp.set_mul(twisted_edwards_params.d, four);
    t.set_mul(t, temp);
  }

  void print() { gmp_printf("(X = %Zd, Y = %Zd, Z = %Zd, T = %Zd)", x.v, y.v, z.v, t.v); }
  void println() { gmp_printf("(X = %Zd, Y = %Zd, Z = %Zd, T = %Zd)\n", x.v, y.v, z.v, t.v); }
  void println(const char *label) {
    gmp_printf("%s: (X = %Zd, Y = %Zd, Z = %Zd, T = %Zd)\n", label, x.v, y.v, z.v, t.v);
  }
  void dump() {
    x.dumpToWords("x");
    y.dumpToWords("y");
    z.dumpToWords("y");
    t.dumpToWords("y");
  }

  void copy_from_rust_type(const g1_affine_t &affine) {
    // TODO(fyquah): Handle infinities
    x.set((uint32_t *)affine.x.data, false);
    y.set((uint32_t *)affine.y.data, false);
    dump();

    affineWeierstrassToExtendedTwistedEdwards();
  }

  void copy_to_rust_type(g1_projective_t &projective) {
    println();
    x.copy_to_rust_type(projective.x);
    y.copy_to_rust_type(projective.y);
    z.copy_to_rust_type(projective.z);
  }
};

}  // namespace bls12_377_g1

#endif
