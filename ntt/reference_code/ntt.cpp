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

void NTT_2_3_in_place(GF (&in)[8]) {
	const int n = 8;
	const int logn = 3;

	typedef ap_uint<logn> INDEX;

	GF tmp;
	for (INDEX k = INDEX(0);;) {
		INDEX rk = k;
		rk.reverse();
		if (k < rk) {
			tmp = in[rk];
			in[rk] = in[k];
			in[k] = tmp;
		}

		k++;
		if (k.iszero()) { // overflow
			break;
		}
	}

	int m = 1;
	for (int i = 1; i <= logn; i++) {
		// w_m is 2^i-th root of unity
		GF w_m = OMEGA[i];

		int k = 0;
		while (k < n) {
			// w = w_m^j at the start of every loop iteration
			GF w = ONE;

			for (int j = 0; j < m; j++) {
				GF t = in[k + j + m];
				t = mult(t, w);

				GF tmp = in[k + j];
				tmp = sub(tmp, t);

				in[k + j + m] = tmp;
				in[k + j] = add(in[k + j], t);

				w = mult(w, w_m);
			}

			k += 2 * m;
		}

		m *= 2;
	}
}

void NTT_2_12_in_place(GF (&in)[4096]) {
	const int n = 4096;
	const int logn = 12;

	typedef ap_uint<logn> INDEX;

	GF tmp;
	for (INDEX k = INDEX(0);;) {
		INDEX rk = k;
		rk.reverse();
		if (k < rk) {
			tmp = in[rk];
			in[rk] = in[k];
			in[k] = tmp;
		}

		k++;
		if (k.iszero()) { // overflow
			break;
		}
	}

	int m = 1;
	for (int i = 1; i <= logn; i++) {
		// w_m is 2^i-th root of unity
		GF w_m = OMEGA[i];

		int k = 0;
		while (k < n) {
			// w = w_m^j at the start of every loop iteration
			GF w = ONE;

			for (int j = 0; j < m; j++) {
				GF t = in[k + j + m];
				t = mult(t, w);

				GF tmp = in[k + j];
				tmp = sub(tmp, t);

				in[k + j + m] = tmp;
				in[k + j] = add(in[k + j], t);

				w = mult(w, w_m);
			}

			k += 2 * m;
		}

		m *= 2;
	}
}

