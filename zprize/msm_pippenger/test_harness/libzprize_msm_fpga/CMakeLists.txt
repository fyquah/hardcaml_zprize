# Standard CMake prelude - nothing special
cmake_minimum_required(VERSION 3.0)
project(LibZprizeMsmFpga CXX)

add_subdirectory(host)

add_library(zprize_msm_fpga STATIC zprize_msm_fpga.cpp)
target_link_libraries(zprize_msm_fpga INTERFACE gmp PRIVATE msm_compute)

# Here is not-so-standard element.
# The target installation command has to exist
# and has to install all linkable targets 
# into `.` (current) directory for cargo to find it.  
install(TARGETS zprize_msm_fpga DESTINATION .)
