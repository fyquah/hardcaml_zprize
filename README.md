# ZPrize submissions

This repo has submissions to two of the ZPrize tracks, in the `zprize` folder.

## Accelerating MSM Operations on FPGA

We have implemented a optimized version of pippengers algorithm for calculating
MSM, see the README.md [here](zprize/msm_pippenger/README.md) for more details.


## Accelerating NTT Operations on FPGA

Click [here for the writeup for our NTT submission](https://fyquah.github.io/hardcaml_zprize/zprize/zprize_ntt_top.html).

The code in this repository that concerns NTT are primarily resides in
- `zprize/ntt`
- `libs/hardcaml_ntt`

While the code does use other support libraries (eg: `libs/hardcaml_ntt` and
`libs/vits_infrastructure`), they are not part of the core functionality of the
NTT core.

# Compiling the bls12-377 reference

Run `cargo build` in `libs/rust/ark_bls12_377_g1` to compile the dynamic library
exposing a reference implementation of the bls12-377 g1 curve. This is
necessary for the expect tests to work expectedly.

z3 should be installed to run tests.

# Compiling OCaml and Hardcaml code

The RTL (Verilog) used by various flows in this repo is written using the
[Hardcaml](https://github.com/janestreet/hardcaml) library, which needs OCaml
installed to generate.

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

# Contributing

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
dual licensed as below, without any additional terms or conditions.

# License

Copyright 2022 Andy Ray, Ben Devlin, Fu Yong Quah, Rahul Yesantharao.

This project is licensed under either of

- [Apache License, Version 2.0](https://www.apache.org/licenses/LICENSE-2.0) ([`LICENSE-APACHE`](LICENSE-APACHE))
- [MIT license](https://opensource.org/licenses/MIT) ([`LICENSE-MIT`](LICENSE-MIT))

at your option.

The [SPDX](https://spdx.dev) license identifier for this project is `MIT OR Apache-2.0`.
