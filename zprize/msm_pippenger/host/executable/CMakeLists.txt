cmake_minimum_required(VERSION 3.12)
set(CMAKE_CXX_STANDARD 17)

file(GLOB SRCS *.cpp)

# program to perform precomputation + stream points in
add_executable(host_start hostStart.cpp)
target_link_libraries(host_start PRIVATE msm_compute)

# program to receive bucket sums + do final computation
add_executable(host_finish hostFinish.cpp)
target_link_libraries(host_finish PRIVATE msm_compute)

# program to generate constants
add_executable(generate_constants generateConstants.cpp)
target_link_libraries(generate_constants PRIVATE msm_compute)
