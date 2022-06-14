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

#include "arithmetic.hpp"

using namespace std;

// Add two Goldilocks elements together
//
// The number may not be between 0..MODULUS - 1,
// it can be between MODULUS - 1..2^64-1.
GF add(GF left, GF right) {
	GF res = left + right;

	bool overflow = res.get_bit(64);
	res.set_bit(64, false);
	if(overflow) {
		res = res + EPSILON;
	}

	overflow = res.get_bit(64);
	res.set_bit(64, false);
	if(overflow) {
		res = res + EPSILON;
	}

	return res;
}

// Subtract a Goldilocks element from another one
//
// The number may not be between 0..MODULUS - 1,
// it can be between MODULUS - 1..2^64-1.
GF sub(GF left, GF right) {
	GF res = left - right;

	bool underflow = res.get_bit(64);
	res.set_bit(64, false);
	if(underflow) {
		res = res - EPSILON;
	}

	underflow = res.get_bit(64);
	res.set_bit(64, false);
	if(underflow) {
		res = res - EPSILON;
	}

	return res;
}

// Negate a Goldilocks element
GF negate(GF x) {
	if(x.is_zero()){
		return x;
	} else {
		return MODULUS - to_canonical(x);
	}
}

// Multiply two Goldilocks elements
GF mult(GF left, GF right) {
	ap_ulong left_u64 = left.to_uint64();
	ap_ulong right_u64 = right.to_uint64();

	GF_MULT left_u128 = GF_MULT(left_u64);
	GF_MULT right_u128 = GF_MULT(right_u64);

	GF_MULT res = left_u128 * right_u128;

	GF res_lo = GF((res & MASK).to_uint64());
	GF res_hi = GF((res >> 64).to_uint64());

	GF res_hi_hi = GF(res_hi >> 32);
    GF res_hi_lo = GF(res_hi & EPSILON);

    GF t0 = res_lo - res_hi_hi;
    bool underflow = t0.get_bit(64);
    t0.set_bit(64, false);
    if(underflow) {
    	t0 = t0 - EPSILON;
    }

    GF t1 = res_hi_lo * EPSILON;

    GF final_res = t0 + t1;
    bool overflow = final_res.get_bit(64);
    final_res.set_bit(64, false);
    if(overflow) {
    	final_res = final_res + EPSILON;
    }

    return final_res;
}

//

GF to_canonical(GF x) {
	if(x >= MODULUS) {
		return x - MODULUS;
	}
	return x;
}
