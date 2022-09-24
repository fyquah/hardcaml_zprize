#!/bin/bash

dune exec zprize/ntt/hardcaml/bin/simulate.exe -- vitis 4 -log-blocks 0
dune exec zprize/ntt/hardcaml/bin/simulate.exe -- vitis 4 -log-blocks 1

dune exec zprize/ntt/hardcaml/bin/simulate.exe -- vitis 5 -log-blocks 0
dune exec zprize/ntt/hardcaml/bin/simulate.exe -- vitis 5 -log-blocks 1
dune exec zprize/ntt/hardcaml/bin/simulate.exe -- vitis 5 -log-blocks 2

dune exec zprize/ntt/hardcaml/bin/simulate.exe -- vitis 6 -log-blocks 0
dune exec zprize/ntt/hardcaml/bin/simulate.exe -- vitis 6 -log-blocks 1
dune exec zprize/ntt/hardcaml/bin/simulate.exe -- vitis 6 -log-blocks 2
dune exec zprize/ntt/hardcaml/bin/simulate.exe -- vitis 6 -log-blocks 3
