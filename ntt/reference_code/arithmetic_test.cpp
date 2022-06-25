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
#include <vector>

using namespace std;

vector<GF> get_test_vector() {
	vector<GF> test_vector;

	for (int i = 0; i < 10; i++) {
		// add the test cases 0..10
		test_vector.push_back(GF(i));

    /* XXX aray: Fixed the constants below (need to bracket the [1 << p] and use 64 bit arithmetic). */

		// add the (1 << 31) - 10..(1 << 31) + 10
		test_vector.push_back(GF((1ull << 31) - 10 + i));
		test_vector.push_back(GF((1ull << 31) + i));

		// add the (1 << 32) - 10..(1 << 32) + 10
		test_vector.push_back(GF((1ull << 32) - 10 + i));
		test_vector.push_back(GF((1ull << 32) + i));

		// add the (1 << 63) - 10..(1 << 63) + 10
    test_vector.push_back(GF((1ull << 63) - 10 + i));
    test_vector.push_back(GF((1ull << 63) + i));

		// add the MODULUS - 10..MODULUS
		test_vector.push_back(MODULUS - GF(10 - i));
	}

  /*
  using Iter = std::vector<GF>::const_iterator;
  for (Iter it=test_vector.begin(); it != test_vector.end(); it++) {
    cout << (*it).to_string(10)<< endl;
  }

  cout << ZERO.to_string(10) << endl;
  cout << ONE.to_string(10) << endl;
  cout << TWO.to_string(10) << endl;
  cout << MODULUS.to_string(10) << endl;
  cout << EPSILON.to_string(10) << endl;
  cout << MASK.to_string(10) << endl;
  */

	return test_vector;
}

int arithmetic_test() {
	cout << "Running arithmetic test." << endl;

	vector<GF> test_vector = get_test_vector();
	for(int i = 0; i < test_vector.size(); i++) {
		for(int j = 0; j < test_vector.size(); j++) {
			GF left = test_vector.at(i);
			GF right = test_vector.at(j);

			while(left >= MODULUS) {
				left -= MODULUS;
			}
			while(right >= MODULUS) {
				right -= MODULUS;
			}

			// check if addition is done correctly
			GF actual = to_canonical(add(left, right));
			GF expected = left + right;
			while(expected >= MODULUS) {
				expected -= MODULUS;
			}

			if(actual != expected){
				cout << "Error: Compute " + left.to_string(10) + " + " + right.to_string(10) + ": expected " + expected.to_string(10) + " actual " + actual.to_string(10) << endl;
				return 1;
			}

			// check if subtraction is done correctly
			actual = to_canonical(sub(left, right));
			if(left >= right) {
				expected = left - right;
			} else {
				expected = MODULUS - (right - left);
			}

			if(actual != expected){
				cout << "Error: Compute " + left.to_string(10) + " - " + right.to_string(10) + ": expected " + expected.to_string(10) + " actual " + actual.to_string(10) << endl;
				return 1;
			}

			// check if multiplication is done correctly
			actual = to_canonical(mult(left, right));
			{
				ap_ulong left_u64 = left.to_uint64();
				ap_ulong right_u64 = right.to_uint64();
				ap_ulong modulus_u64 = MODULUS.to_uint64();

				GF_MULT left_u128 = GF_MULT(left_u64);
				GF_MULT right_u128 = GF_MULT(right_u64);
				GF_MULT modulus_u128 = GF_MULT(modulus_u64);

				GF_MULT res_u128 = (left_u128 * right_u128) % modulus_u128;
				expected = GF(res_u128.to_uint64());
			}
			if(actual != expected){
				cout << "Error: Compute " + left.to_string(10) + " * " + right.to_string(10) + ": expected " + expected.to_string(10) + " actual " + actual.to_string(10) << endl;
				return 1;
			}
		}
	}

	return 0;
}

int main() { arithmetic_test(); return 0; }

