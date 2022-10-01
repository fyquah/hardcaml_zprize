cmake_minimum_required(VERSION 3.13.1)
project(msm_host)

set(CMAKE_CXX_STANDARD 17)
#set(CMAKE_ENABLE_EXPORTS true)

# Issue with clang-tidy - it picks up errors from googletest
#set(CMAKE_CXX_CLANG_TIDY
#clang-tidy-9;
#-header-filter=include/kdtree/.;
#-checks=*;
#-warnings-as-errors=*;)

# Build Types
set(available_build_types Debug Release RelWithDebInfo)
if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE "Debug" CACHE STRING "Build Type ${available_build_types}" FORCE)
  message(STATUS "CMAKE_BUILD_TYPE is not set. Using default")
  message(STATUS "Available build types are: ${available_build_types}")
endif()
message(STATUS "Build type: ${CMAKE_BUILD_TYPE}")

set(CMAKE_CXX_FLAGS "-Wall -Wextra -mcx16 -std=c++17")
set(CMAKE_CXX_FLAGS_DEBUG "-O0 -g -DDEBUG")
set(CMAKE_CXX_FLAGS_RELEASE "-O3 -march=native -DNDEBUG")
set(CMAKE_CXX_FLAGS_RELWITHDEBINFO "${CMAKE_CXX_FLAGS_RELEASE} -g")

message(STATUS "--------------- General configuration -------------")
message(STATUS "CMake Generator:                ${CMAKE_GENERATOR}")
message(STATUS "Compiler:                       ${CMAKE_CXX_COMPILER_ID} ${CMAKE_CXX_COMPILER_VERSION}")
message(STATUS "Build type:                     ${CMAKE_BUILD_TYPE}")
message(STATUS "CMAKE_CXX_FLAGS:                ${CMAKE_CXX_FLAGS}")
message(STATUS "CMAKE_CXX_FLAGS_DEBUG:          ${CMAKE_CXX_FLAGS_DEBUG}")
message(STATUS "CMAKE_CXX_FLAGS_RELEASE:        ${CMAKE_CXX_FLAGS_RELEASE}")
message(STATUS "CMAKE_CXX_FLAGS_RELWITHDEBINFO: ${CMAKE_CXX_FLAGS_RELWITHDEBINFO}")
message(STATUS "CMAKE_EXE_LINKER_FLAGS          ${CMAKE_CXX_LINKER_FLAGS}")
message(STATUS "CMAKE_INSTALL_PREFIX:           ${CMAKE_INSTALL_PREFIX}" )

# ------ External ------
add_subdirectory(external)

# ------ Libraries -----

add_subdirectory(naive_driver)
add_subdirectory(msm_compute)

# ------ Tests and executables ------
enable_testing()
add_subdirectory(executable)
add_subdirectory(test)
add_subdirectory(benchmark)

# CR fyquah for rahulyesantharao: driver probably not the best name?
add_subdirectory(driver)
