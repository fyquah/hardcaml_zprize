/*
 * Copyright (C) 2022 DZK
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#ifndef _ARITHMETIC_FUNC_H_
#define _ARITHMETIC_FUNC_H_

#include <ap_int.h>

typedef ap_uint<65> GF;
const GF EPSILON = GF((1ull << 32) - 1);
const GF ZERO = GF(0);
const GF ONE = GF(1);
const GF TWO = GF(2);
const GF MODULUS = GF(0xFFFFFFFF00000001);

typedef ap_uint<128> GF_MULT;
const GF_MULT MASK = GF_MULT(0xFFFFFFFFFFFFFFFF);

const GF OMEGA[33] = {
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

GF add(GF left, GF right);
GF sub(GF left, GF right);
GF negate(GF x);
GF mult(GF left, GF right);
GF to_canonical(GF x);

#endif
