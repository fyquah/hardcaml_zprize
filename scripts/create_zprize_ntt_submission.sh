#!/bin/bash

set -euo pipefail

OUTPUT_FILENAME="$1"

tar \
  -c \
  -z \
  -f "$OUTPUT_FILENAME" \
  dune-project \
  vitis_infrastructure/common \
  vitis_infrastructure/scripts \
  vitis_infrastructure/template \
  hardcaml_axi \
  zprize/ntt/hardcaml \
  zprize/ntt/host/{*.cpp,*.h,Makefile} \
  zprize/ntt/fpga/ntt-2_12 \
  zprize/ntt/fpga/ntt-2_18 \
  zprize/ntt/fpga/ntt-2_24 \
  zprize/ntt/test/
