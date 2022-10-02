# Compiling the Reference

Run `cargo build` in `libs/rust/ark_bls12_377_g1` to compile the dynamic library
exposing a reference implementation of the bls12-377 g1 curve. This is
necessary for the expect tests to work expectedly.

z3 should be installed to run tests.

# Compiling OCaml and Hardcaml code

1. Follow the instructions in https://opam.ocaml.org/doc/Install.html to install
opam, the OCaml package manager
2. Install the OCaml 4.13.1 compiler. You might need to run


```
opam switch create 4.13.1
eval $(opam env) # to pick up relevant environment variables
```

3. Install the relevant OCaml dependencies

```
opam install . --deps-only
```

You might need to install extra packages (m4 gmp-devep libffi-devel
libgmp3-dev), especially on the AWS boxes. In ubuntu, you can run


```
sudo apt-get install m4 gmp-devel libffi-devel libgmp3-dev z3
```


To check everything installed correctly

```
opam exec -- dune build
opam exec -- dune runtest
```

# ZPrize submissions

This repo has submissions to two of the ZPrize tracks, in the `zprize` folder.

## Accelerating MSM Operations on FPGA

We have implemented a optimized version of pippengers algorithm for calculating
MSM, see the README.md [here](zprize/msm_pippenger/README.md) for a detailed
explanation of optimizations and block diagrams.

See the README.md [here](zprize/msm_pippenger/test_fpga_harness/README.md) for
instructions on benchmarking our solution.


## Accelerating NTT Operations on FPGA
