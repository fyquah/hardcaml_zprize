#!/bin/bash

set -euo pipefail

make -C ../host test_random.exe
cp ../host/test_random.exe ./

# xsim requires xrt.ini requires absolute directory for pre-run tcl scripts.
sed -e "s#CURRENT_DIRECTORY#$PWD#g" xrt.template.ini >xrt.ini

./test_random.exe \
    --xclbin ../fpga/ntt-2_18-normal_layout/build/build_dir.hw.xilinx_u55n_gen3x4_xdma_2_202110_1/ntt_fpga.xclbin \
    --num-test-cases 10 \
    --core-type NTT-2_18 \
    --log-blocks 0 \
    --memory-layout NORMAL_LAYOUT
