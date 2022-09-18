cmake_minimum_required(VERSION 3.12)
set(CMAKE_CXX_STANDARD 17)

# googlebenchmark requires at least 3.16.3, so we compile benchmarks only if
# we're on that version.
if(${CMAKE_VERSION} VERSION_GREATER "3.16.2")
  # check whether googletest is locally installed, if not download and fetch
  message(STATUS "--------------- GoogleBenchmark -------------")
  find_package(benchmark CONFIG)
  if(NOT benchmark_FOUND)
      # new way of including googletest
      # Download and unpack googletest at configure time
      set(BENCHMARK_ENABLE_LIBPFM ON)
      set(BENCHMARK_ENABLE_GTEST_TESTS OFF)
      configure_file(CMakeLists.txt.in googlebenchmark-download/CMakeLists.txt)
      execute_process(COMMAND ${CMAKE_COMMAND} -G "${CMAKE_GENERATOR}" .
              RESULT_VARIABLE result
              WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/googlebenchmark-download )
      if(result)
          message(FATAL_ERROR "CMake step for googlebenchmark failed: ${result}")
      endif()
      execute_process(COMMAND ${CMAKE_COMMAND} --build . --config Release
              RESULT_VARIABLE result
              WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/googlebenchmark-download )
      if(result)
          message(FATAL_ERROR "Build step for googlebenchmark failed: ${result}")
      endif()
  
      # Prevent overriding the parent project's compiler/linker
      # settings on Windows
      #set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
  
      # Add googletest directly to our build. This defines
      # the gtest and gtest_main targets.
      add_subdirectory(${CMAKE_BINARY_DIR}/googlebenchmark-src
              ${CMAKE_BINARY_DIR}/googlebenchmark-build
              EXCLUDE_FROM_ALL)
      set(benchmark_LIBRARIES "benchmark")
      set(benchmark_main_LIBRARIES "benchmark_main")
  else()
      message(STATUS "using locally installed GoogleBenchmark")
      #set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
      set(benchmark_LIBRARIES benchmark::benchmark)
  endif()
  
  message(STATUS "--------------- Building Benchmarks -------------")
  set(NUMA_COMMAND numactl -i all)
  function(add_benchmark NAME)
    add_executable(bench_${NAME} bench_${NAME}.cpp)
    target_link_libraries(bench_${NAME} PRIVATE msm_compute ${benchmark_main_LIBRARIES})
    if (CMAKE_CXX_COMPILER_ID STREQUAL "AppleClang")
      # -march=native not supported by clang on my M1 mac
      target_compile_options(bench_${NAME} PRIVATE -Wall -Wextra -Wfatal-errors)
    else ()
      target_compile_options(bench_${NAME} PRIVATE -Wall -Wextra -Wfatal-errors -march=native)
    endif()
  
    # numactl
    add_custom_target(numactl_bench_${NAME}
      COMMAND ${NUMA_COMMAND} ${CMAKE_CURRENT_BINARY_DIR}/bench_${NAME} --benchmark_counters_tabular=true --benchmark_out_format=json
    )
    add_dependencies(numactl_bench_${NAME} bench_${NAME})
  endfunction()
  
  # Add benchmark file
  add_benchmark(point_arithmetic)
  
  # copy resources
  #file(COPY datasets DESTINATION ${CMAKE_BINARY_DIR}/benchmark)
endif()
