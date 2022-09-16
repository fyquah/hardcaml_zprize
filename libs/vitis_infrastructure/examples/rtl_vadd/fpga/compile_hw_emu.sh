#!/bin/bash

set -x

../../../scripts/run_build.py  --top-level-name vadd --platform varium-c1100 --build-dir build --build-target hw_emu kernel_vadd
