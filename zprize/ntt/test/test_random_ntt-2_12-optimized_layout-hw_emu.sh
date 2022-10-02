#!/bin/bash

set -euo pipefail

make -C ../host test_random.exe
cp ../host/test_random.exe ./
cp ../fpga/ntt-2_12-optimized-layout/build/_x.hw_emu.xilinx_u55n_gen3x4_xdma_2_202110_1/emconfig.json ./

# xsim requires xrt.ini requires absolute directory for pre-run tcl scripts.
sed -e "s#CURRENT_DIRECTORY#$PWD#g" xrt.template.ini >xrt.ini

XCL_EMULATION_MODE=hw_emu ./test_random.exe \
    --xclbin ../fpga/ntt-2_12-optimized-layout/build/build_dir.hw_emu.xilinx_u55n_gen3x4_xdma_2_202110_1/ntt_fpga.xclbin \
    --core-type NTT-2_12 \
    --memory-layout OPTIMIZED_LAYOUT
