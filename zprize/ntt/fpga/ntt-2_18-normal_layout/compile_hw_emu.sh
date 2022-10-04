#!/bin/bash

set -x

dune build @default

../../../../libs/vitis_infrastructure/scripts/run_build.py \
	--top-level-name ntt_fpga \
	--platform varium-c1100 \
	--build-dir build \
	--build-target hw_emu \
	--cfg ntt_fpga.cfg \
	krnl_controller:cpp \
	krnl_ntt:rtl
