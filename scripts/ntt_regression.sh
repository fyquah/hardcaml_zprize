#!/bin/bash

dune exec zprize/ntt/hardcaml/bin/simulate.exe -- vitis 4 -log-blocks 0 -memory-layout normal
dune exec zprize/ntt/hardcaml/bin/simulate.exe -- vitis 4 -log-blocks 1 -memory-layout normal

dune exec zprize/ntt/hardcaml/bin/simulate.exe -- vitis 5 -log-blocks 0 -memory-layout normal
dune exec zprize/ntt/hardcaml/bin/simulate.exe -- vitis 5 -log-blocks 1 -memory-layout normal
dune exec zprize/ntt/hardcaml/bin/simulate.exe -- vitis 5 -log-blocks 2 -memory-layout normal

dune exec zprize/ntt/hardcaml/bin/simulate.exe -- vitis 6 -log-blocks 0 -memory-layout normal
dune exec zprize/ntt/hardcaml/bin/simulate.exe -- vitis 6 -log-blocks 1 -memory-layout normal
dune exec zprize/ntt/hardcaml/bin/simulate.exe -- vitis 6 -log-blocks 2 -memory-layout normal
dune exec zprize/ntt/hardcaml/bin/simulate.exe -- vitis 6 -log-blocks 3 -memory-layout normal

dune exec zprize/ntt/hardcaml/bin/simulate.exe -- vitis 4 -log-blocks 0 -memory-layout optimised
dune exec zprize/ntt/hardcaml/bin/simulate.exe -- vitis 4 -log-blocks 1 -memory-layout optimised

dune exec zprize/ntt/hardcaml/bin/simulate.exe -- vitis 5 -log-blocks 0 -memory-layout optimised
dune exec zprize/ntt/hardcaml/bin/simulate.exe -- vitis 5 -log-blocks 1 -memory-layout optimised
dune exec zprize/ntt/hardcaml/bin/simulate.exe -- vitis 5 -log-blocks 2 -memory-layout optimised

dune exec zprize/ntt/hardcaml/bin/simulate.exe -- vitis 6 -log-blocks 0 -memory-layout optimised
dune exec zprize/ntt/hardcaml/bin/simulate.exe -- vitis 6 -log-blocks 1 -memory-layout optimised
dune exec zprize/ntt/hardcaml/bin/simulate.exe -- vitis 6 -log-blocks 2 -memory-layout optimised
dune exec zprize/ntt/hardcaml/bin/simulate.exe -- vitis 6 -log-blocks 3 -memory-layout optimised
