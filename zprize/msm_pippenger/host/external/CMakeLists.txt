cmake_minimum_required(VERSION 3.12)
set(CMAKE_CXX_STANDARD 17)

include(ExternalProject)

# include gmplib
set(GMP_PREFIX ${CMAKE_CURRENT_BINARY_DIR}/gmp-6.2.1)
ExternalProject_Add(gmp_external
  URL https://ftp.gnu.org/gnu/gmp/gmp-6.2.1.tar.xz
  TIMEOUT 5
  PREFIX ${GMP_PREFIX}
  CONFIGURE_COMMAND ${GMP_PREFIX}/src/gmp_external/configure --prefix=${GMP_PREFIX} --enable-cxx --disable-static --enable-shared
  TEST_COMMAND make check
  TEST_BEFORE_INSTALL 1
  BUILD_COMMAND make
)
ExternalProject_Get_Property(gmp_external INSTALL_DIR)
set(gmp_INSTALL_DIR ${INSTALL_DIR})

add_library(gmp SHARED stub.cpp)
set_property(TARGET gmp PROPERTY POSITION_INDEPENDENT_CODE ON)
add_dependencies(gmp gmp_external)
target_include_directories(gmp PUBLIC "${gmp_INSTALL_DIR}/include/")
target_link_libraries(gmp PUBLIC "${gmp_INSTALL_DIR}/lib/libgmp.so")
