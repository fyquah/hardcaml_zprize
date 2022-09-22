cmake_minimum_required(VERSION 3.13.1)
project(msm_host_cxx)
set(CMAKE_CXX_STANDARD 17)
# This cmake file is based on this guide to call C++ from rust
# https://flames-of-code.netlify.app/blog/rust-and-cmake-cplusplus/

add_subdirectory(host)

# Here is not-so-standard element.
# The target installation command has to exist
# and has to install all linkable targets 
# into `.` (current) directory for cargo to find it.  
install(TARGETS driver DESTINATION .)


# install(TARGETS gmp DESTINATION .)
install(
  FILES ${CMAKE_BINARY_DIR}/host/external/gmp-6.2.1/lib/libgmp.so
  ${CMAKE_BINARY_DIR}/host/external/gmp-6.2.1/lib/libgmpxx.so
  DESTINATION .)
