#!/bin/bash

set -euo pipefail

# Make sure all the binaries are built
mkdir -p ../host/build
cd ../host/build
cmake3 .. 
make -j

cd ../../test

# Copy these files local so Vivado does not complain
cp ../fpga/build/_x.hw_emu.xilinx_aws-vu9p-f1_shell-v04261818_201920_3/emconfig.json ./
cp ../host/build/driver/host_buckets ./

# xsim requires xrt.ini requires absolute directory for pre-run tcl scripts.
sed -e "s#CURRENT_DIRECTORY#$PWD#g" xrt.template.ini >xrt.ini

# Run this to get the input points file.
dune exec -- ../hardcaml/bin/tools.exe test-vectors \
  -num-points 8 \
  -input-points-filename input.points \
  -input-scalars-filename input.scalars \
  -output-filename output.points \
  -seed 0 

XCL_EMULATION_MODE=hw_emu ./host_buckets ../fpga/build/build_dir.hw_emu.xilinx_aws-vu9p-f1_shell-v04261818_201920_3/msm_pippenger.xclbin input.points input.scalars output.points
