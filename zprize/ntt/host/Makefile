COMMON_DIR=../../../libs/vitis_infrastructure/common
CXXFLAGS+=-I$(XILINX_XRT)/include -I$(XILINX_VIVADO)/include -Wall -O3 -g -std=c++1y
CXXFLAGS+=-I$(COMMON_DIR)/includes/xcl2
CXXFLAGS+=-fmessage-length=0
CXXFLAGS+=-L/opt/xilinx/xrt/lib -pthread -lOpenCL -lrt -lstdc++ -lxrt_coreutil
COMMON_SRCS=$(COMMON_DIR)/includes/xcl2/xcl2.cpp reference.cpp driver.cpp ntt_preprocessing.o ntt_postprocessing.o

all: test_random.exe evaluate_given.exe bench_throughput.exe bench_latency.exe

test_random.exe: test_random.cpp $(COMMON_SRCS) driver.h gf.h reference.h
	g++ -o $@ $< $(COMMON_SRCS) $(CXXFLAGS) 

evaluate_given.exe: evaluate_given.cpp $(COMMON_SRCS) driver.h gf.h reference.h
	g++ -o $@ $< $(COMMON_SRCS) $(CXXFLAGS) 

bench_throughput.exe: bench_throughput.cpp $(COMMON_SRCS) driver.h gf.h reference.h
	g++ -o $@ $< $(COMMON_SRCS) $(CXXFLAGS) 

bench_latency.exe: bench_latency.cpp $(COMMON_SRCS) driver.h gf.h reference.h
	g++ -o $@ $< $(COMMON_SRCS) $(CXXFLAGS) 
