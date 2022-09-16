COMMON_DIR=../../../libs/vitis_infrastructure/common
CXXFLAGS+=-I$(XILINX_XRT)/include -I$(XILINX_VIVADO)/include -Wall -O0 -g -std=c++1y
CXXFLAGS+=-I$(COMMON_DIR)/includes/xcl2
CXXFLAGS+=-fmessage-length=0
CXXFLAGS+=-L/opt/xilinx/xrt/lib -pthread -lOpenCL -lrt -lstdc++ -luuid -lxrt_coreutil -lxilinxopencl
EXECUTABLE=host.exe
HOST_SRCS=$(COMMON_DIR)/includes/xcl2/xcl2.cpp host.cpp

$(EXECUTABLE): $(HOST_SRCS)
	g++ -o $@ $(HOST_SRCS) $(CXXFLAGS) 
