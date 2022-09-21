cmake_minimum_required(VERSION 3.12)
set(CMAKE_CXX_STANDARD 17)

set(COMMON_DIR ${CMAKE_CURRENT_SOURCE_DIR}/../../../../libs/vitis_infrastructure/common)

message(STATUS "In driver/")
# program to perform precomputation + stream points in
add_executable(host_msm host_msm.cpp ${COMMON_DIR}/includes/xcl2/xcl2.cpp)

# add include directories
target_include_directories(host_msm
	PUBLIC $ENV{XILINX_XRT}/include
	PUBLIC $ENV{XILINX_VIVADO}/include
	PUBLIC ${COMMON_DIR}/includes/xcl2)

# add libraries to link against
target_link_directories(host_msm PUBLIC /opt/xilinx/xrt/lib)
target_link_libraries(host_msm PUBLIC OpenCL rt uuid xrt_coreutil xilinxopencl msm_compute gmp) # stdc++ 

# program compute the raw fpga output against a reference
add_executable(host_buckets host_buckets.cpp ${COMMON_DIR}/includes/xcl2/xcl2.cpp)

# add include directories
target_include_directories(host_buckets
	PUBLIC $ENV{XILINX_XRT}/include
	PUBLIC $ENV{XILINX_VIVADO}/include
	PUBLIC ${COMMON_DIR}/includes/xcl2)

# add libraries to link against
target_link_directories(host_buckets PUBLIC /opt/xilinx/xrt/lib)
target_link_libraries(host_buckets PUBLIC OpenCL rt uuid xrt_coreutil xilinxopencl msm_compute gmp) # stdc++ 

# main driver program
add_library(driver STATIC host_buckets.cpp ${COMMON_DIR}/includes/xcl2/xcl2.cpp)

# add include directories
target_include_directories(driver
	PUBLIC $ENV{XILINX_XRT}/include
	PUBLIC $ENV{XILINX_VIVADO}/include
	PUBLIC ${COMMON_DIR}/includes/xcl2)

# add libraries to link against
target_link_directories(driver PUBLIC /opt/xilinx/xrt/lib)
target_link_libraries(driver PUBLIC OpenCL rt uuid xrt_coreutil xilinxopencl msm_compute gmp) # stdc++ 


message(STATUS "Finished driver/")
