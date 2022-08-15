COMMON_DIR=../../../vitis_infrastructure/common
CXXFLAGS+=-I$(XILINX_XRT)/include -I$(XILINX_VIVADO)/include -Wall -O3 -g -std=c++1y
CXXFLAGS+=-I$(COMMON_DIR)/includes/xcl2
CXXFLAGS+=-fmessage-length=0
CXXFLAGS+=-L/opt/xilinx/xrt/lib -pthread -lOpenCL -lrt -lstdc++ -lxrt_coreutil
HOST_SRCS=$(COMMON_DIR)/includes/xcl2/xcl2.cpp test_random.cpp reference.cpp driver.cpp

test_random.exe: $(HOST_SRCS) driver.h gf.h reference.h
	g++ -o $@ $(HOST_SRCS) $(CXXFLAGS) 
