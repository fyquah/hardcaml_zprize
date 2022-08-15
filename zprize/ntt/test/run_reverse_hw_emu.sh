#!/bin/bash

set -euo pipefail

make -C ../host host.exe

# fyquah: I think vitis requires emconfig.json to be in the same directory as host.exe,
# so copy both things here.

cp ../fpga/reverse/build/_x.hw_emu.xilinx_u55n_gen3x4_xdma_2_202110_1/emconfig.json ./
cp ../host/host.exe ./

# xsim requires xrt.ini requires absolute directory for pre-run tcl scripts.
sed -e "s#CURRENT_DIRECTORY#$PWD#g" xrt.template.ini >xrt.ini

XCL_EMULATION_MODE=hw_emu \
    ./host.exe \
    --xclbin ../fpga/reverse/build/build_dir.hw.xilinx_u55n_gen3x4_xdma_2_202110_1/ntt_fpga.xclbin \
    --core-type REVERSE \
    --log-row-size 6
