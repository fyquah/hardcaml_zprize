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

#include "ntt.hpp"
#include <iostream>

using namespace std;

int test_2_3_linear() {
	GF in[8];
	for (int i = 0; i < 8; i++) {
		in[i] = ZERO;
	}

	in[0] = ONE;
	in[1] = TWO;

	NTT_2_3_in_place(in);

	GF expected_out[8] = { GF(0x0000000000000003), GF(0xfffffffefe000002), GF(
			0x0002000000000001), GF(0xfffffdff00000202), GF(0xffffffff00000000),
			GF(0x0000000002000001), GF(0xfffdffff00000002), GF(
					0x000001fffffffe01) };

	for (int i = 0; i < 8; i++) {
		if (in[i] != expected_out[i]) {
			cout << "Error: NTT result [" << i << "] should be "
					<< expected_out[i].to_string(10) << ", but the output is "
					<< in[i].to_string(10) << endl;
			return 1;
		}
	}

	return 0;
}

int test_2_12_linear() {
	GF in[4096];
	for (int i = 0; i < 4096; i++) {
		in[i] = ZERO;
	}

	in[0] = ONE;
	in[1] = TWO;

	NTT_2_12_in_place(in);

	// we only test the first eight elements
	GF expected_out[8] = { GF(0x0000000000000003), GF(0xe586a3342b3bf96c), GF(
			0x0ca769003b43919f), GF(0x28b1a9691a680e3c), GF(0x3b1e55b017fdb2e4),
			GF(0x309d8a339a00ae6a), GF(0xdc13ebf6fd47c483), GF(
					0xc12decfb84bb920e) };

	for (int i = 0; i < 8; i++) {
		if (in[i] != expected_out[i]) {
			cout << "Error: NTT result [" << i << "] should be "
					<< expected_out[i].to_string(10) << ", but the output is "
					<< in[i].to_string(10) << endl;
			return 1;
		}
	}

	return 0;
}

int test_2_3_fully_random() {
	GF in[8] = { GF(0xcef967e3e1d0860e), GF(0x44be7570bcd4f9df), GF(
			0xf4848ed283e858f2), GF(0xa3a3a47eeb6f76f6), GF(0xa12d1d0b69c4108b),
			GF(0xeb285d19459ef6c3), GF(0x10d812558ad9c103), GF(
					0xd19d3e319d1b6b4a) };
	NTT_2_3_in_place(in);

	GF expected_out[8] = { GF(0x1aaadb56e555836b), GF(0x975bcb9d395a282f), GF(
			0x69055db04cf94815), GF(0x963cdab11477cc1c), GF(0xd05b70dbcf57ddad),
			GF(0xed14bc2fbdc30962), GF(0x6c8e69de2cabb133), GF(
					0x9c83c8e1d49cd861) };

	for (int i = 0; i < 8; i++) {
		if (in[i] != expected_out[i]) {
			cout << "Error: NTT result [" << i << "] should be "
					<< expected_out[i].to_string(10) << ", but the output is "
					<< in[i].to_string(10) << endl;
			return 1;
		}
	}

	return 0;
}

int ntt_test() {
	cout << "Running NTT test." << endl;

	int res = test_2_3_linear();
	if (res != 0) {
		return res;
	}

	res = test_2_12_linear();
	if (res != 0) {
		return res;
	}

	res = test_2_3_fully_random();
	if (res != 0) {
		return res;
	}

	return 0;
}

int main() { ntt_test(); return 0; }
