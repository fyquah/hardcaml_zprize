#!/bin/bash

# Simple short test failing pippenger with the is_in_pipeline optimisation
dune exec libs/pippenger/bin/simulate.exe -- random -affine 32 -datapath-depth 10 -num-windows 2 -window 5 12 -debug -seed 2

dune exec libs/pippenger/bin/simulate.exe  -- random -affine-point-bits 32 -datapath-depth 10 -num-windows 2 -window-size-bits 5 10_000 
dune exec libs/pippenger/bin/simulate.exe  -- random -affine-point-bits 32 -datapath-depth 20 -num-windows 2 -window-size-bits 8 10_000 
dune exec libs/pippenger/bin/simulate.exe  -- random -affine-point-bits 32 -datapath-depth 20 -num-windows 3 -window-size-bits 8 10_000 
dune exec libs/pippenger/bin/simulate.exe  -- random -affine-point-bits 32 -datapath-depth 20 -num-windows 4 -window-size-bits 8 10_000 
dune exec libs/pippenger/bin/simulate.exe  -- random -affine-point-bits 32 -datapath-depth 211 -num-windows 3 -window-size-bits 12 100_000 
dune exec libs/pippenger/bin/simulate.exe  -- random -affine-point-bits 32 -datapath-depth 211 -num-windows 3 -window-size-bits 12 100_000 -stall
