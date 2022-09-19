#!/bin/bash

set -x
mkdir afi
cd afi
date_time=$(date +%Y%m%d)

$VITIS_DIR/tools/create_vitis_afi.sh \
       -xclbin=../build/build_dir.hw.xilinx_aws-vu9p-f1_shell-v04261818_201920_3/msm_pippenger.link.xclbin \
       -s3_bucket=55312.bsdevlin \
       -s3_dcp_key="${date_time}_dcp" \
       -s3_logs_key="${date_time}_logs"

