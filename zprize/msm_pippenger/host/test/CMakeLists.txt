cmake_minimum_required(VERSION 3.12)
set(CMAKE_CXX_STANDARD 17)

# check whether googletest is locally installed, if not download and fetch
message(STATUS "--------------- GoogleTest -------------")
find_package(GTest CONFIG)
if(NOT GTest_FOUND)
    # new way of including googletest
    # Download and unpack googletest at configure time
    configure_file(CMakeLists.txt.in googletest-download/CMakeLists.txt)
    execute_process(COMMAND ${CMAKE_COMMAND} -G "${CMAKE_GENERATOR}" .
            RESULT_VARIABLE result
            WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/googletest-download )
    if(result)
        message(FATAL_ERROR "CMake step for googletest failed: ${result}")
    endif()
    execute_process(COMMAND ${CMAKE_COMMAND} --build .
            RESULT_VARIABLE result
            WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/googletest-download )
    if(result)
        message(FATAL_ERROR "Build step for googletest failed: ${result}")
    endif()

    # Prevent overriding the parent project's compiler/linker
    # settings on Windows
    set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)

    # Add googletest directly to our build. This defines
    # the gtest and gtest_main targets.
    add_subdirectory(${CMAKE_BINARY_DIR}/googletest-src
            ${CMAKE_BINARY_DIR}/googletest-build
            EXCLUDE_FROM_ALL)
    set(GTest_LIBRARIES "gtest")
else()
    message(STATUS "using locally installed GoogleTest")
    set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
    set(GTest_LIBRARIES GTest::gtest)
endif()

#file(COPY resources DESTINATION ${CMAKE_BINARY_DIR}/test)

# ADD TESTS
file(GLOB SRCS *.cpp)
include(GoogleTest)
add_executable(test_msm ${SRCS})
target_link_libraries(test_msm PUBLIC ${GTest_LIBRARIES} PRIVATE
  msm_compute)

gtest_discover_tests(test_msm)
