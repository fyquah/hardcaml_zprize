#!/bin/bash

set -x

../../../scripts/run_build.py \
	--top-level-name vadd \
	--platform varium-c1100 \
	--build-dir build \
	--build-target hw_emu \
	--cfg adder.cfg \
	krnl_input_stage_rtl \
	krnl_adder_stage_rtl \
	krnl_output_stage_rtl \
