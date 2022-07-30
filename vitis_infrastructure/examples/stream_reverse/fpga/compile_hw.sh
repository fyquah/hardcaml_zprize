#!/bin/bash

set -x

../../../scripts/run_build.py \
	--top-level-name reverse_stream \
	--platform varium-c1100 \
	--build-dir build \
	--build-target hw \
	--cfg reverse_stream.cfg \
	krnl_controller:cpp \
	krnl_reverse:cpp
