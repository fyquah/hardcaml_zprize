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

#include <iostream>

int arithmetic_test();
int ntt_test();

using namespace std;

int main(){
	int res = arithmetic_test();
	if(res != 0){
		return res;
	}

	res = ntt_test();
	if(res != 0){
		return res;
	}

	cout << "All test passed." << endl;
	return 0;
}
