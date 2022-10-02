#ifndef CONSTANTS
#define CONSTANTS
#include "bls12_377_g1.h"

// just to generate constants
namespace bls12_377_g1 {
const char Q_STR[] =
    "0x1ae3a4617c510eac63b05c06ca1493b1a22d9f300f5138f1ef3622fba094800170b5d443"
    "00000008508c00000000"
    "001";

// Weierstrass Parameters
const char S_STR[] =
    "0x19d47d415b5ff60a87a8b7bbab25eb6427dd58ca38e47030efd1e6310ac7bf3079221bf2"
    "b4bd72c5106e9e70fcc6"
    "156";

const char TWISTED_SCALE_STR[] =
    "0x363b01df81a0405e86206f2cc504d147752b91b664d91d4c82fed217902ee87435448583"
    "6fa3ccba3af539c17385"
    "7b";

// Twisted Edwards Parameters
const char A_STR[] =
    "0x1ae3a4617c510eac63b05c06ca1493b1a22d9f300f5138f1ef3622fba094800170b5d443"
    "00000008508c00000000"
    "000";

const char D_STR[] =
    "0xe2dcedff103e7161354a88156e4b00fe66a526a0237cfe5f683497c9afb7635d5c9307f7"
    "8e160e14f2b6d4bd9490"
    "04";

const char K_STR[] =
    "0x177f95e65b6bf7fc2f8f4fbe3b4cc6e2aa705a3f51e66d9fdd06ffd95626c6a3adc8cbbf"
    "1c2c1ba4dcada97b2920"
    "07";

const char C_A_STR[] =
    "0x32d756062d349e59416ece15ccbf8e86ef0d33183465a42fe2cb65fc1664272e6bb28f0e"
    "1c7a7c9c05824ad09adc"
    "01";
const char C_B_STR[] =
    "0x19d47d415b5ff60a87a8b7bbab25eb6427dd58ca38e47030efd1e6310ac7bf3079221bf2"
    "b4bd72c5106e9e70fcc6"
    "156";

static void generateWords(const char *name, const char *str) {
  bls12_377_g1::GFq p(str);
  p.dumpToWords(name);
}

void generateAllWords() {
  generateWords("ZERO", "0x0");
  generateWords("ONE", "0x1");
  generateWords("TWO", "0x2");
  generateWords("THREE", "0x3");
  generateWords("Q", Q_STR);
  generateWords("TWISTED_SCALE", TWISTED_SCALE_STR);
  generateWords("K", K_STR);
  generateWords("S", S_STR);
  generateWords("A", A_STR);
  generateWords("D", D_STR);
  generateWords("C_A", C_A_STR);
  generateWords("C_B", C_B_STR);
}
}  // namespace bls12_377_g1

#endif
