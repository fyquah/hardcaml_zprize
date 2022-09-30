add_library(naive_driver SHARED naive_driver.cpp)
target_link_libraries(naive_driver PUBLIC gmp PRIVATE msm_compute)
