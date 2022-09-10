# ------ Library ------
add_library(msm_compute INTERFACE)
set(MSM_COMPUTE_INCLUDE_DIR "include/")
target_include_directories(msm_compute INTERFACE ${MSM_COMPUTE_INCLUDE_DIR})
target_link_libraries(msm_compute INTERFACE gmp)
target_compile_features(msm_compute INTERFACE cxx_std_17)
