
#ifndef BLS12_377_G1
#define BLS12_377_G1

#include <stdio.h>
#include <gmp.h>
#include <cstdint>

namespace bls12_377_g1 {

const char Q_STR[] =
    "0x01ae3a4617c510eac63b05c06ca1493b1a22d9f300f5138f1ef3622fba094800170b5d44300000008508c0000000"
    "0001";
const char D_STR[] =
    "0xe2dcedff103e7161354a88156e4b00fe66a526a0237cfe5f683497c9afb7635d5c9307f78e160e14f2b6d4bd9490"
    "04";
const int NUM_BITS = 377;
const int NUM_32B_WORDS = (NUM_BITS + 31) / 32;

const uint32_t ZERO_WORDS[NUM_32B_WORDS] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
const uint32_t ONE_WORDS[NUM_32B_WORDS] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1};

mpz_t q;
static void init_empty(mpz_t v) {
  mpz_init2(v, NUM_BITS + mp_bits_per_limb);  // https://gmplib.org/manual/Initializing-Integers
}

class GFq {
 public:
  mpz_t v;
  // assumes [NUM_32B_WORDS] words
  explicit GFq(const uint32_t words[]) {
    init_empty(v);

    const int WORDS_MOST_SIGNIFICANT = 1;
    // const int WORDS_LEAST_SIGNIFICANT = -1;
    const int BYTES_MOST_SIGNIFICANT = 1;
    const int BYTES_LEAST_SIGNIFICANT = -1;
    mpz_import(v, NUM_32B_WORDS, WORDS_MOST_SIGNIFICANT, sizeof(uint32_t), BYTES_LEAST_SIGNIFICANT,
               0, words);
  }

  // convenience wrappers for constants
  explicit GFq(const char str[]) {
    init_empty(v);
    mpz_set_str(v, str, 0);
  }
  explicit GFq(long int val) {
    init_empty(v);
    mpz_set_si(v, val);
  }

  // does not set a value
  GFq() {
    printf("start constructor\n");
    init_empty(v);
    printf("end constructor\n");
  }

  ~GFq() { mpz_clear(v); }

  // arithmetic
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

  // debugging
  void print() { gmp_printf("%Zd", v); }
};

// constants
const GFq d(D_STR);
const GFq two(2);
GFq k;

static bool initialized = false;
void init() {
#ifndef NDEBUG
  if (initialized) throw("double initialize");
  initialized = true;
#endif

  // initialize q
  init_empty(q);
  mpz_set_str(q, Q_STR, 0);

  // initialize k
  k.set_mul(d, two);
}

void check_init() {
#ifndef NDEBUG
  if (!initialized) throw("not initialized");
#endif
}

class Xyzt {
 public:
  GFq x, y, z, t;

  Xyzt(const uint32_t words_x[], const uint32_t words_y[], const uint32_t words_z[],
       const uint32_t words_t[])
      : x(words_x), y(words_y), z(words_z), t(words_t) {}

  // Initialize to identity
  Xyzt() : Xyzt(ZERO_WORDS, ONE_WORDS, ONE_WORDS, ZERO_WORDS) {}

  // TODO(rayesantharao): add scaled identity for precompute

  void addInto(const Xyzt &other) {
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
    A.set_mul(temp1, temp2);

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

  void print() { gmp_printf("(X = %Zd, Y = %Zd, Z = %Zd, T = %Zd)", x.v, y.v, z.v, t.v); }
  void println() { gmp_printf("(X = %Zd, Y = %Zd, Z = %Zd, T = %Zd)\n", x.v, y.v, z.v, t.v); }
  void println(const char *label) {
    gmp_printf("%s: (X = %Zd, Y = %Zd, Z = %Zd, T = %Zd)\n", label, x.v, y.v, z.v, t.v);
  }
};

}  // namespace bls12_377_g1

#endif
