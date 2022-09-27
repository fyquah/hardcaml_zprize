cmake_minimum_required(VERSION 3.12)
set(CMAKE_CXX_STANDARD 17)

get_filename_component(CUR_REAL_PATH ${CMAKE_CURRENT_SOURCE_DIR}/ REALPATH)
set(COMMON_DIR ${CUR_REAL_PATH}/../../../../libs/vitis_infrastructure/common)

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
add_library(driver SHARED driver.cpp ${COMMON_DIR}/includes/xcl2/xcl2.cpp)

# add include directories
target_include_directories(driver
	PUBLIC $ENV{XILINX_XRT}/include
	PUBLIC $ENV{XILINX_VIVADO}/include
	PUBLIC ${COMMON_DIR}/includes/xcl2)

# add libraries to link against
target_link_directories(driver PUBLIC /opt/xilinx/xrt/lib)
target_link_libraries(driver PUBLIC OpenCL rt uuid xrt_coreutil xilinxopencl gmp PRIVATE msm_compute) # stdc++


message(STATUS "Finished driver/")
