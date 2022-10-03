#!/bin/bash

set -euo pipefail

make -C ../host bench_throughput.exe
cp ../host/bench_throughput.exe ./

# xsim requires xrt.ini requires absolute directory for pre-run tcl scripts.
sed -e "s#CURRENT_DIRECTORY#$PWD#g" xrt.template.ini >xrt.ini

./bench_throughput.exe \
    --xclbin ../fpga/ntt-2_24-optimized_layout-64_cores/build/build_dir.hw.xilinx_u55n_gen3x4_xdma_2_202110_1/ntt_fpga.xclbin \
    --num-evaluations 200 \
    --core-type NTT-2_24 \
    --log-blocks 3 \
    --memory-layout OPTIMIZED_LAYOUT
