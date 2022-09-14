#!/bin/bash

set -euo pipefail

dune build @default
vivado -mode batch -source ../../../scripts/run_vivado.tcl -tclargs naive_pipe_add
