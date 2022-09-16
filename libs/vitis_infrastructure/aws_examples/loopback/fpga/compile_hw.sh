#!/bin/bash

set -x

../../../scripts/run_build.py \
	--top-level-name loopback \
	--platform aws \
	--build-dir build \
	--build-target hw \
	--cfg loopback.cfg \
	krnl_mm2s:cpp \
        krnl_loopback \
	krnl_s2mm:cpp \
