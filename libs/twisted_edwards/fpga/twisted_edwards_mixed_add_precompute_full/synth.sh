#!/bin/bash

dune build @default
vivado -mode batch -source ../../../../scripts/run_vivado.tcl -tclargs twisted_edwards_mixed_add_precompute_full
