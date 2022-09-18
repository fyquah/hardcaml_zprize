cmake_minimum_required(VERSION 3.18)
set(CMAKE_CXX_STANDARD 17)

set(COMMON_DIR ../../../libs/vitis_infrastructure/common)

# program to perform precomputation + stream points in
add_executable(host host_msm.cpp ${COMMON_DIR}/includes/xcl2/xcl2.cpp)


# add include directories
target_include_directories($ENV{XILINX_XRT}/include)
target_include_directories($ENV{XILINX_VIVADO}/include)
target_include_directories(${COMMON_DIR}/includes/xcl2)

# add libraries to link against
target_link_directories(host PUBLIC /opt/xilinx/xrt/lib)
target_link_libraries(host PUBLIC OpenCL rt uuid xrt_coreutil xilinxopencl) # stdc++ 
