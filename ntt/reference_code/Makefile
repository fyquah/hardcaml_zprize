# XXX aray: this is not portable.
# We currently need vivado for some HLS headers (ap_uint basically).
VIVADO=~/xilinx/Vivado/2020.1/include

all: arithmetic_test ntt_test

clean:
	rm -f arithmetic_test ntt_test

arithmetic_test: arithmetic.cpp ntt.cpp arithmetic_test.cpp
	g++ -I ${VIVADO} arithmetic.cpp ntt.cpp arithmetic_test.cpp -o arithmetic_test

ntt_test: arithmetic.cpp ntt.cpp ntt_test.cpp
	g++ -I ${VIVADO} arithmetic.cpp ntt.cpp ntt_test.cpp -o ntt_test
