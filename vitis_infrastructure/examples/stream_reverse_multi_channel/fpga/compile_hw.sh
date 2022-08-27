#!/bin/bash

set -x

../../../scripts/run_build.py \
	--top-level-name stream_reverse \
	--platform varium-c1100 \
	--build-dir build \
	--build-target hw \
	--cfg stream_reverse.cfg \
	krnl_controller:cpp \
	krnl_reverse:cpp
