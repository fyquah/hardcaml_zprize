
#ifndef BLS12_377_G1
#define BLS12_377_G1

#include <gmp.h>
#include <stdio.h>

#include <cassert>
#include <cstdint>
#include <cstring>
#include <iostream>

#include "rust_types.h"

namespace bls12_377_g1 {

const int SCALAR_NUM_BITS = 253;
const int NUM_BITS = 377;
const int NUM_64B_WORDS = (NUM_BITS + 63) / 64;
const int NUM_32B_WORDS = (NUM_BITS + 31) / 32;

const uint64_t ZERO_WORDS[NUM_64B_WORDS] = {0, 0, 0, 0, 0, 0};
const uint64_t ONE_WORDS[NUM_64B_WORDS] = {1, 0, 0, 0, 0, 0};
const uint64_t COFACTOR_WORDS[NUM_64B_WORDS] = {
    202099033278250856ULL,  5854854902718660529ULL, 11492539364873682930ULL,
    8885205928937022213ULL, 5545221690922665192ULL, 39800542322357402ULL};
const uint64_t TWO_WORDS[NUM_64B_WORDS] = {2, 0, 0, 0, 0, 0};
const uint64_t FOUR_WORDS[NUM_64B_WORDS] = {4, 0, 0, 0, 0, 0};

// parameters
const uint64_t Q_WORDS[NUM_64B_WORDS] = {
    9586122913090633729ULL, 1660523435060625408ULL,  2230234197602682880ULL,
    1883307231910630287ULL, 14284016967150029115ULL, 121098312706494698ULL};
const uint64_t TWISTED_SCALE_WORDS[NUM_64B_WORDS] = {
    13419307668111328635ULL, 8373674422391776204ULL, 5513249072146820840ULL,
    5149069653798344989ULL,  6811167148309021905ULL, 15264527973195840ULL};
const uint64_t K_WORDS[NUM_64B_WORDS] = {
    11879560860523896839ULL, 11794303776563407899ULL, 11519371117816325830ULL,
    16332990531524224621ULL, 18171900594119986374ULL, 6614206255950839ULL};
const uint64_t S_WORDS[NUM_64B_WORDS] = {
    5838611145544196438ULL, 545535609814112044ULL,   1080052896469122035ULL,
    4791220378608224003ULL, 12140169109060083382ULL, 116328141604388704ULL};
const uint64_t A_WORDS[NUM_64B_WORDS] = {
    9586122913090633728ULL, 1660523435060625408ULL,  2230234197602682880ULL,
    1883307231910630287ULL, 14284016967150029115ULL, 121098312706494698ULL};
const uint64_t D_WORDS[NUM_64B_WORDS] = {
    1509469849952489476ULL,  6727413605812016654ULL, 6874802657709504355ULL,
    18331520918572203262ULL, 7004586743780231936ULL, 63856259481222769ULL};
const uint64_t C_A_WORDS[NUM_64B_WORDS] = {
    11242535302639311873ULL, 3344963475739540092ULL, 3450543903400682535ULL,
    9723004633616770468ULL,  6431543574269837198ULL, 14310513306317982ULL};
const uint64_t C_B_WORDS[NUM_64B_WORDS] = {
    5838611145544196438ULL, 545535609814112044ULL,   1080052896469122035ULL,
    4791220378608224003ULL, 12140169109060083382ULL, 116328141604388704ULL};
static void init_empty(mpz_t v) {
  mpz_init(v);
  // mpz_init2(v, 384 + mp_bits_per_limb);  //
  // https://gmplib.org/manual/Initializing-Integers
}
static const int WORDS_MOST_SIGNIFICANT = 1;
static const int WORDS_LEAST_SIGNIFICANT = -1;
static const int BYTES_MOST_SIGNIFICANT = 1;
static const int BYTES_LEAST_SIGNIFICANT = -1;
static void set_words(mpz_t v, const uint64_t words[]) {
  assert(NUM_64B_WORDS == 6);
  mpz_import(v, NUM_64B_WORDS, WORDS_LEAST_SIGNIFICANT, sizeof(uint64_t),
             BYTES_LEAST_SIGNIFICANT, 0, (uint64_t *)words);
}

static void set_32b_words(mpz_t v, const uint32_t words[]) {
  assert(NUM_32B_WORDS == 12);
  mpz_import(v, NUM_32B_WORDS, WORDS_LEAST_SIGNIFICANT, sizeof(uint32_t),
             BYTES_LEAST_SIGNIFICANT, 0, (uint32_t *)words);
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

  void set(const uint64_t words[]) { set_words(v, words); }
  void set_32b(const uint32_t words[]) { set_32b_words(v, words); }
  void set(const GFq &other) { mpz_set(v, other.v); }
  // assumes [NUM_64B_WORDS] words
  explicit GFq(const uint64_t words[]) {
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

  // Calling the copy constructor will make a new copy of mpz_t with
  // identical values.
  GFq(const GFq &other) {
    init_empty(v);
    mpz_set(v, other.v);
  }

  ~GFq() { mpz_clear(v); }

  GFq &operator=(const GFq &) = delete;
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

  void copy_to_buffer(void *b) {
    mpz_export(
        /* rop */ b,
        /* countp */ nullptr,
        /* order */ WORDS_LEAST_SIGNIFICANT,
        /* size */ sizeof(uint32_t),
        /* endian */ BYTES_LEAST_SIGNIFICANT,
        /* nails */ 0,
        /* op */ v);
  }

  void copy_to_rust_type(biginteger384_t &b) {
    uint64_t temp[NUM_64B_WORDS];
    memset(temp, 0, sizeof(temp));
    mpz_export(
        /* rop */ temp,
        /* countp */ nullptr,
        /* order */ WORDS_LEAST_SIGNIFICANT,
        /* size */ sizeof(uint64_t),
        /* endian */ BYTES_LEAST_SIGNIFICANT,
        /* nails */ 0,
        /* op */ v);
    for (int i = 0; i < NUM_64B_WORDS; i++) {
      // invert: NUM_64B_WORDS - 1 - i
      b.data[i] = temp[i];
    }
  }

  // debugging
  void print() { gmp_printf("%Zd", v); }

  void dumpToWords(const char *str) {
    uint64_t words[NUM_64B_WORDS];
    size_t countp;
    assert(sizeof(words) == NUM_64B_WORDS * 8);
    memset(words, 0, sizeof(words));

    mpz_export(words, &countp, WORDS_LEAST_SIGNIFICANT, sizeof(uint64_t),
               BYTES_LEAST_SIGNIFICANT, 0, v);
    assert(countp <= NUM_64B_WORDS);
    printf("const uint64_t %s_WORDS[NUM_64B_WORDS] = {", str);
    for (int i = 0; i < NUM_64B_WORDS; i++) {
      if (i > 0) {
        printf(", ");
      }
      printf("%luULL", words[i]);
    }
    printf("};\n");
  }

  bool operator==(const GFq &other) const { return mpz_cmp(v, other.v) == 0; }

  bool operator==(const int &other) const { return mpz_cmp_si(v, other) == 0; }

  bool operator!=(const GFq &other) const { return mpz_cmp(v, other.v) != 0; }

  bool operator!=(const int &other) const { return mpz_cmp_si(v, other) != 0; }
};

// parameter constants
const GFq twisted_scale(TWISTED_SCALE_WORDS);
const GFq one(1);
const GFq two(2);
const GFq three(3);
const GFq four(4);
const GFq k(K_WORDS);
const GFq COFACTOR(COFACTOR_WORDS);

struct WeierstrassParams {
  GFq a, b, s, alpha;
  WeierstrassParams(const uint64_t a_words[], const uint64_t b_words[],
                    const uint64_t s_words[], const uint64_t alpha_words[])
      : a(a_words), b(b_words), s(s_words), alpha(alpha_words) {}

  void println() const {
    gmp_printf(
        "WeierstrassParams: {\n\ta = %#Zx,\n\tb = %#Zx,\n\ts = %#Zx,\n\talpha "
        "= %#Zx\n}\n",
        a.v, b.v, s.v, alpha.v);
  }
};
// a = -1 in twisted edwards, so A_WORDS is -1
const WeierstrassParams weierstrass_params(ZERO_WORDS, ONE_WORDS, S_WORDS,
                                           A_WORDS);

struct MontgomeryParams {
  GFq c_A, c_B;
  MontgomeryParams(const uint64_t c_A_words[], const uint64_t c_B_words[])
      : c_A(c_A_words), c_B(c_B_words) {}
  void println() const {
    gmp_printf("MontgomeryParams: {\n\tc_A = %#Zx,\n\tc_B = %#Zx\n}\n", c_A.v,
               c_B.v);
  }
};
const MontgomeryParams montgomery_params(C_A_WORDS, C_B_WORDS);

struct TwistedEdwardsParams {
  GFq a, d;
  TwistedEdwardsParams(const uint64_t a_words[], const uint64_t d_words[])
      : a(a_words), d(d_words) {}
  void println() const {
    gmp_printf("TwistedEdwardsParams: {\n\ta = %#Zx,\n\td = %#Zx\n}\n", a.v,
               d.v);
  }
};

const TwistedEdwardsParams twisted_edwards_params(A_WORDS, D_WORDS);

void print_params() {
  weierstrass_params.println();
  montgomery_params.println();
  twisted_edwards_params.println();
  gmp_printf("Other: {\n\tk = %#Zx, \n\ttwisted_scale = %#Zx\n}\n", k.v,
             twisted_scale.v);
}

struct GeneralUnifiedAddIntoTemps {
  GFq A;
  GFq B;
  GFq C;
  GFq D;
  GFq E;
  GFq F;
  GFq G;
  GFq H;
  GFq temp1;
  GFq temp2;
};

class Xyzt {
 public:
  GFq x, y, z, t;

  Xyzt(const uint64_t words_x[], const uint64_t words_y[],
       const uint64_t words_z[], const uint64_t words_t[])
      : x(words_x), y(words_y), z(words_z), t(words_t) {}

  Xyzt() : Xyzt(ZERO_WORDS, ONE_WORDS, ONE_WORDS, ZERO_WORDS) {}
  Xyzt(const Xyzt &other) : x(other.x), y(other.y), z(other.z), t(other.t) {
    // printf(" *** RAHUL: construct from another\n");
    // fflush(stdout);
  }

  Xyzt &operator=(const Xyzt &) = delete;
  void set(const Xyzt &other) {
    x.set(other.x);
    y.set(other.y);
    z.set(other.z);
    t.set(other.t);
  }

  void set_32b(const uint32_t words_x[], const uint32_t words_y[],
               const uint32_t words_z[], const uint32_t words_t[]) {
    x.set_32b(words_x);
    y.set_32b(words_y);
    z.set_32b(words_z);
    t.set_32b(words_t);
  }

  /* CR-soon fyquah for bdevlin: Simplify this once the FPGA streams it in
   * 64-bit aligned results.
   */
  void import_from_fpga_vector(const uint32_t packed_repr[]) {
    x.set_32b(packed_repr + NUM_32B_WORDS * 0);
    y.set_32b(packed_repr + NUM_32B_WORDS * 1);
    z.set_32b(packed_repr + NUM_32B_WORDS * 2);
    t.set_32b(packed_repr + NUM_32B_WORDS * 3);
  }

  bool is_z_zero() const { return mpz_cmp_si(z.v, 0) == 0; }

  void setToTwistedEdwardsIdentity() {
    x.set(ZERO_WORDS);
    y.set(ONE_WORDS);
    z.set(ONE_WORDS);
    t.set(ZERO_WORDS);
  }

  void setToWeierstrassInfinity() { z.set(ZERO_WORDS); }

  // conversions
  void twistedEdwardsAffineToExtended() {
    z.set(ONE_WORDS);
    t.set_mul(x, y);
  }
  void twistedEdwardsExtendedToAffine() {
    if (z == 0) {
      printf("z = 0 in twisted edwards extended; something went wrong?\n");
      x.set(ZERO_WORDS);
      y.set(ONE_WORDS);
      z.set(ONE_WORDS);
      t.set(ZERO_WORDS);
    } else {
      x.set_div(x, z);
      y.set_div(y, z);
      z.set(ONE_WORDS);
      t.set(ZERO_WORDS);
    }
  }
  void affineWeierstrassToMontgomery() {
    x.set_sub(x, weierstrass_params.alpha);
    x.set_mul(x, weierstrass_params.s);
    y.set_mul(weierstrass_params.s, y);
  }
  void affineMontgomeryToWeierstrass() {
    GFq temp;
    temp.set_mul(three, montgomery_params.c_B);
    temp.set_div(montgomery_params.c_A, temp);
    x.set_div(x, montgomery_params.c_B);
    x.set_add(x, temp);
    y.set_div(y, montgomery_params.c_B);
    z.set(ONE_WORDS);
  }

  // if the mapping is not possible, leave the point unchanged and return false.
  bool affineMontgomeryToTwistedEdwards() {
    GFq temp1, temp2, temp3;
    temp3.set_mul(twisted_scale, x);
    temp1.set_sub(x, one);
    temp2.set_add(x, one);
    if ((y == 0) || (temp2 == 0)) {
      return false;
    }
    x.set_div(temp3, y);
    y.set_div(temp1, temp2);
    return true;
  }
  bool affineTwistedEdwardsToMontgomery() {
    GFq temp1, temp2;
    temp1.set_add(one, y);
    temp2.set_sub(one, y);
    if (((temp2 == 0) || (x == 0))) {
      // printf("Twisted Edwards -> Montgomery Undefined; Infinity!\n");
      return false;
    }
    temp1.set_div(temp1, temp2);
    temp2.set_div(twisted_scale, x);
    x.set(temp1);
    y.set_mul(temp1, temp2);
    return true;
  }

  // preprocess function - if the point cannot be converted to twisted edwards
  // form, leave it unchanged and return false.
  bool affineWeierstrassToExtendedTwistedEdwards() {
    affineWeierstrassToMontgomery();
    bool convertable = affineMontgomeryToTwistedEdwards();
    if (!convertable) {
      affineMontgomeryToWeierstrass();
      return false;
    }
    twistedEdwardsAffineToExtended();
    return true;
  }
  // postprocess function
  void extendedTwistedEdwardsToWeierstrass() {
    twistedEdwardsExtendedToAffine();
    bool convertible = affineTwistedEdwardsToMontgomery();
    if (!convertible) {
      setToWeierstrassInfinity();
    } else {
      affineMontgomeryToWeierstrass();
    }
  }

  void extendedTwistedEdwardsToWeierstrassInMontgomerySpace() {
    twistedEdwardsExtendedToAffine();
    bool convertible = affineTwistedEdwardsToMontgomery();
    if (!convertible) {
      setToWeierstrassInfinity();
      return;
    }

    affineMontgomeryToWeierstrass();
    x.set_mul(x, COFACTOR);
    y.set_mul(y, COFACTOR);
    z.set(COFACTOR);
  }

  void weistrassValuesInMontgomerySpace() {
    if (z != 0) {
      x.set_mul(x, COFACTOR);
      y.set_mul(y, COFACTOR);
      z.set(COFACTOR);
    }
  }

  void generalUnifiedAddInto(const Xyzt &other,
                             GeneralUnifiedAddIntoTemps &temps) {
    // https://hyperelliptic.org/EFD/g1p/auto-twisted-extended-1.html#addition-add-2008-hwcd-3
    auto &A = temps.A;
    auto &B = temps.B;
    auto &C = temps.C;
    auto &D = temps.D;
    auto &E = temps.E;
    auto &F = temps.F;
    auto &G = temps.G;
    auto &H = temps.H;
    auto &temp1 = temps.temp1;
    auto &temp2 = temps.temp2;

    // A
    temp1.set_sub(y, x);
    temp2.set_sub(other.y, other.x);
    A.set_mul(temp1, temp2);

    // B
    temp1.set_add(y, x);
    temp2.set_add(other.y, other.x);
    B.set_mul(temp1, temp2);

    // C - https://gmplib.org/manual/Efficiency (better to multiply
    // out-of-place)
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

  void weierstrassDoubleInPlace() {
    if (z == 0) {  // point at infinity stays at infinity
      return;
    }
    GFq t1, t2, t3, t4;
    // point doubling
    t1.set_mul(three, x);
    t1.set_mul(t1, x);
    t1.set_add(t1, weierstrass_params.a);
    t2.set_mul(two, y);
    if (t2 == 0) {  // result is infinity
      setToWeierstrassInfinity();
      return;
    }
    t3.set_div(t1, t2);  // t3 = (3 * x1^2 + a) / (2 * y1)

    t2.set_add(x, x);
    t2.set_add(t2, x);  // t2 = (2 * x1 + x1)
    t2.set_mul(t2, t3); // t2 = t2 * t3

    t4.set_mul(t3, t3);  // t4 = (3 * x1^2 + a)^2 / (2 * y1)^2
    t1.set_sub(t4, x);   // t1 = t4 - x1
    t1.set_sub(t1, x);   // t1 = t1 - x1

    t3.set_mul(t3, t4);  // t3 = t3 * t4 = (3 * x1^2 + a)^3 / (2 * y1)^3
    t2.set_sub(t2, t3);  // t2 = t2 - t3
    t2.set_sub(t2, y);   // t2 = t2 - y1

    x.set(t1);
    y.set(t2);
  }

  void weierstrassAddition(const Xyzt &other) {
    // https://hyperelliptic.org/EFD/g1p/auto-shortw.html
    if (z == 0) {  // adding to identity results in other
      x.set(other.x);
      y.set(other.y);
      z.set(other.z);
      t.set(other.t);
      return;
    }

    GFq t1, t2, t3, t4;
    if (other == *this) {
      weierstrassDoubleInPlace();
    } else if (other.z == 0) {
      // other is infinity, do nothing
    } else {
      // point addition
      // compute x
      t1.set_sub(other.y, y);
      t2.set_sub(other.x, x);
      if (t2 == 0) {
        setToWeierstrassInfinity();
        return;
      }
      t3.set_div(t1, t2);  // (y2 - y1) / (x2 - x1)

      t2.set_add(x, x);
      t2.set_add(t2, other.x);  // (2 * x1 + x2)
      t2.set_mul(t3, t2);

      t4.set_mul(t3, t3);
      t1.set_sub(t4, x);
      t1.set_sub(t1, other.x);  // x3

      t3.set_mul(t3, t4);
      t2.set_sub(t2, t3);
      t2.set_sub(t2, y);

      x.set(t1);
      y.set(t2);
    }
  }

  void generalUnifiedAddInto(const Xyzt &other) {
    GeneralUnifiedAddIntoTemps temps;
    generalUnifiedAddInto(other, temps);
  }

  void doubleInPlace(GeneralUnifiedAddIntoTemps &temps) {
    generalUnifiedAddInto(*this, temps);
  }

  void doubleInPlace() {
    GeneralUnifiedAddIntoTemps temps;
    generalUnifiedAddInto(*this, temps);
  }

  void postComputeFPGA(GeneralUnifiedAddIntoTemps &temps) {
    // x1 = (y0 - x0) / 4
    // y1 = (y0 + x0) / 4
    // z1 = z0 / 4
    // t1 = t0
    GFq &temp1 = temps.temp1;
    GFq &temp2 = temps.temp2;

    temp1.set_sub(y, x);
    temp1.divBy4();

    temp2.set_add(y, x);
    temp2.divBy4();

    x.set(temp1);
    y.set(temp2);
    z.divBy4();
  }

  void postComputeFPGA() {
    GeneralUnifiedAddIntoTemps temps;
    postComputeFPGA(temps);
  }

  void preComputeFPGA(GeneralUnifiedAddIntoTemps &temps) {
    // (x, y, z, t) -> ((y-x)/2,(y+x)/2,4d*t)
    GFq &temp = temps.temp1;

    temp.set_sub(y, x);
    temp.set_div(temp, two);
    y.set_add(y, x);
    y.set_div(y, two);
    x.set(temp);

    temp.set_mul(twisted_edwards_params.d, four);
    t.set_mul(t, temp);
  }

  void preComputeFPGA() {
    GeneralUnifiedAddIntoTemps temps;
    preComputeFPGA(temps);
  }

  void print() {
    gmp_printf("(X = %Zd, Y = %Zd, Z = %Zd, T = %Zd)", x.v, y.v, z.v, t.v);
  }
  void println() {
    gmp_printf("(X = %Zd, Y = %Zd, Z = %Zd, T = %Zd)\n", x.v, y.v, z.v, t.v);
  }
  void println_hex() {
    gmp_printf("(X = %#Zx, Y = %#Zx, Z = %#Zx, T = %#Zx)\n", x.v, y.v, z.v,
               t.v);
  }
  void println(const char *label) {
    gmp_printf("%s: (X = %Zd, Y = %Zd, Z = %Zd, T = %Zd)\n", label, x.v, y.v,
               z.v, t.v);
  }
  void dump() {
    x.dumpToWords("x");
    y.dumpToWords("y");
    z.dumpToWords("z");
    t.dumpToWords("t");
  }

  bool copy_from_rust_type(const g1_affine_t &affine) {
    if (affine.infinity) {
      // Special representation for infinity in the twisted edwards curve
      setToTwistedEdwardsIdentity();
      return true;
    }

    // import the affine weierstrass representation
    x.set((uint64_t *)affine.x.data);
    x.set_div(x, COFACTOR);
    y.set((uint64_t *)affine.y.data);
    y.set_div(y, COFACTOR);
    return affineWeierstrassToExtendedTwistedEdwards();
  }

  void copy_to_fpga_buffer(uint32_t *b) {
    x.copy_to_buffer((void *)(b + 0 * NUM_32B_WORDS));
    y.copy_to_buffer((void *)(b + 1 * NUM_32B_WORDS));
    t.copy_to_buffer((void *)(b + 2 * NUM_32B_WORDS));
  }

  void copy_to_rust_type(g1_projective_t &projective) {
    // printf("FINAL RESULT, COPYING TO RUST\n");
    // fflush(stdout);
    // println();
    // println_hex();
    // dump();

    // printf("\n\n normalized point\n\n");
    if (z != 0) {
      // if z = 0, then it'll just be the identity
      Xyzt temp;
      temp.x.set_div(x, z);
      temp.y.set_div(y, z);
      temp.z.set(ONE_WORDS);
      temp.t.set(ZERO_WORDS);
    }
    // temp.println_hex();
    // temp.dump();
    // printf("\nPARAMS\n");
    // print_params();
    x.copy_to_rust_type(projective.x);
    y.copy_to_rust_type(projective.y);
    z.copy_to_rust_type(projective.z);
  }

  bool operator==(const Xyzt &other) const {
    return x == other.x && y == other.y && z == other.z && t == other.t;
  }

  bool operator!=(const Xyzt &other) const {
    return x != other.x || y != other.y || z != other.z || t != other.t;
  }
};

void weierstrassMultiplication(Xyzt &base, const biginteger256_t &scalar) {
  Xyzt temp;
  temp.set(base);
  base.setToWeierstrassInfinity();

  // printf("    ** RAHUL: doing multiplication: %p\n", &base);
  // fflush(stdout);
  // printf("    ** RAHUL: setting temp: %p\n", &base);
  // fflush(stdout);
  // temp.set(base);
  // printf("    ** RAHUL: created multiplication temp\n");
  // fflush(stdout);

  for (int i = 0; i < SCALAR_NUM_BITS; i++) {
    // printf("%d ", i);
    // if (i % 32 == 0) printf("\n");
    if (scalar.getBit(i)) {
      base.weierstrassAddition(temp);
    }
    temp.weierstrassDoubleInPlace();
  }
  // printf("DONE \n");
  // fflush(stdout);
}

void weierstrassMultiplyAndAdd(Xyzt &base, const Xyzt &point,
                               const biginteger256_t &scalar) {
  static Xyzt temp;
  temp.set(point);

  // printf(" ** RAHUL : MaA created temp: %p\n", &temp);
  // fflush(stdout);
  // temp.println();

  weierstrassMultiplication(temp, scalar);
  // printf(" ** RAHUL : MaA did multiplication\n");
  // fflush(stdout);

  base.weierstrassAddition(temp);
  // printf(" ** RAHUL : MaA completed\n");
  // fflush(stdout);
}

}  // namespace bls12_377_g1

#endif
