#!/bin/bash

set -euo pipefail

make -C ../host host.exe

# fyquah: I think vitis requires emconfig.json to be in the same directory as host.exe,
# so copy both things here.

cp ../fpga/build/_x.hw_emu.xilinx_aws-vu9p-f1_shell-v04261818_201920_3/emconfig.json ./
cp ../host/host.exe ./

# xsim requires xrt.ini requires absolute directory for pre-run tcl scripts.
sed -e "s#CURRENT_DIRECTORY#$PWD#g" xrt.template.ini >xrt.ini

# Run this to get the input point file!
dune exec -- ../hardcaml/bin/tools.exe test-vectors -input input.points -num 8

XCL_EMULATION_MODE=hw_emu ./host.exe ../fpga/build/build_dir.hw_emu.xilinx_aws-vu9p-f1_shell-v04261818_201920_3/msm_pippenger.xclbin input.points
