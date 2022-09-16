#!/bin/bash

set -euo pipefail

OUTPUT_FILENAME="$1"

shopt -s extglob

tar \
  -c \
  -z \
  -f "$OUTPUT_FILENAME" \
  dune-project \
  libs/vitis_infrastructure/common \
  libs/vitis_infrastructure/scripts \
  libs/vitis_infrastructure/template \
  libs/hardcaml_axi \
  zprize/ntt/hardcaml \
  zprize/ntt/host/{*.cpp,*.h,Makefile} \
  zprize/ntt/fpga/ntt-2_12/!(*@(build)) \
  zprize/ntt/fpga/ntt-2_18/!(*@(build)) \
  zprize/ntt/fpga/ntt-2_24/!(*@(build)) \
  zprize/ntt/fpga/ntt-2_18/build/build_dir.hw.xilinx_u55n_gen3x4_xdma_2_202110_1/ntt_fpga.xclbin \
  zprize/ntt/fpga/ntt-2_24/build/build_dir.hw.xilinx_u55n_gen3x4_xdma_2_202110_1/ntt_fpga.xclbin \
  zprize/ntt/test/
