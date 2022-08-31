cmake_minimum_required(VERSION 3.12)
set(CMAKE_CXX_STANDARD 17)

include(ExternalProject)

# include gmplib
set(GMP_PREFIX ${CMAKE_CURRENT_BINARY_DIR}/gmp-6.2.1)
ExternalProject_Add(gmp
  URL https://ftp.gnu.org/gnu/gmp/gmp-6.2.1.tar.xz
  TIMEOUT 5
  PREFIX ${GMP_PREFIX}
  CONFIGURE_COMMAND ${GMP_PREFIX}/src/gmp/configure --prefix=${GMP_PREFIX}/src/gmp-build --enable-cxx
  TEST_COMMAND make check
  TEST_BEFORE_INSTALL 1
  BUILD_COMMAND make
)
ExternalProject_Get_Property(gmp INSTALL_DIR)
set(gmp_INSTALL_DIR ${INSTALL_DIR})
