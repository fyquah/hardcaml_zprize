# ZPrize submissions

This repo has submissions to two of the ZPrize tracks, in the `zprize` folder.

## Accelerating MSM Operations on FPGA

We have implemented an optimized version of Pippengers algorithm for calculating
MSM, see the [README.md](zprize/msm_pippenger/README.md) for more details and
instructions on building from source.

The top level code for compiling an FPGA image and Verilog (using Hardcaml)
resides in `zprize/msm_pippenger`. Underlying controller and field operations
are taken from the libraries in:
- `libs/pippenger`
- `libs/twisted_edwards`
- `libs/field_ops`


## Accelerating NTT Operations on FPGA

Click [here for the writeup for our NTT submission](https://fyquah.github.io/hardcaml_zprize/zprize/zprize_ntt_top.html).

The code in this repository that concerns NTT primarily resides in
- `zprize/ntt`
- `libs/hardcaml_ntt`

While the code does use other support libraries (eg: `libs/hardcaml_ntt` and
`libs/vits_infrastructure`), they are not part of the core functionality of the
NTT core.

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
libgmp3-dev), especially on the AWS boxes. In ubuntu you can run the following
command. For Centos on the AWS boxes, use `sudo yum install`.

```
sudo apt-get install m4 gmp-devel libffi-devel libgmp3-dev z3 cargo
```

To check everything installed correctly

```
opam exec -- dune build
opam exec -- dune runtest
```

Now please continue to the relevant section listed [above](#zprize-submissions)
to build the top level designs from source.

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
