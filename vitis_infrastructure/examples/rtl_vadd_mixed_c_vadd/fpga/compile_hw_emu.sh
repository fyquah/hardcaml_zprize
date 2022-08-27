#!/bin/bash

set -x

../../../scripts/run_build.py \
	--top-level-name rtl_vadd_mixed_c_vadd \
       	--platform varium-c1100 \
	--build-dir build \
	--build-target hw_emu \
	krnl_vadd:cpp \
	krnl_vadd_rtl
