#!/bin/bash

set -euo pipefail

make -C ../host bench_latency.exe
cp ../host/bench_latency.exe ./

# xsim requires xrt.ini requires absolute directory for pre-run tcl scripts.
sed -e "s#CURRENT_DIRECTORY#$PWD#g" xrt.template.ini >xrt.ini

# Performs [num_rounds * 8] evaluations concurrently.
./bench_latency.exe \
    --xclbin ../fpga/ntt-2_24-normal_layout-32_cores/build/build_dir.hw.xilinx_u55n_gen3x4_xdma_2_202110_1/ntt_fpga.xclbin \
    --num-evaluations 200 \
    --core-type NTT-2_24 \
    --memory-layout OPTIMIZED_LAYOUT \
    --what-to-measure memcpy-and-pcie-and-fpga-latency \
    --log-blocks 2 \
    $@
