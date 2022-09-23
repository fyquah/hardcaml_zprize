add_library(zprize_msm_fpga SHARED zprize_msm_fpga.cpp)
target_link_libraries(zprize_msm_fpga PUBLIC gmp PRIVATE msm_compute)
