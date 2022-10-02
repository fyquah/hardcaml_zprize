# ZPrize submissions

This repo has submissions to two of the ZPrize tracks, in the `zprize` folder.

## Accelerating MSM Operations on FPGA

We have implemented a optimized version of pippengers algorithm for calculating
MSM, see the README.md [here](zprize/msm_pippenger/README.md) for more details.


# Running on AWS

You need to run these steps on the run box. Make sure you have cloned the aws-fpga repo and run:

```
source ~/aws-fpga/vitis_runtime_setup.sh
```

Check the status of the FPGA:

```
systemctl status mpd
```

You need the .awsxclbin file from the build box, usually the easiest way is to
download this from the s3 bucket or scp it over.

Now you can run the host.exe test program:

```
./host.exe  msm_pippenger.link.awsxclbin input.points output.points
```

## AFI-ids and performance

We have listed all the AFI-ids and their performance at certain points in the repo. Currently the highest performance one is:

afi-066aeb84a7663930a
```
[memcpy-ing scalars to special memory region] 0.254459s
[transferring scalars to gmem] 0.269489s
[Doing FPGA Computation] 5.40025s
[Copying results back from gmem] 0.00128308s
[Doing on-host postprocessing] 0.475065s
```

### All AFIs

AFI-id | AFI-gid | Notes | 2^26 performance
------- | ------- | ----- | -----
 afi-04f8603ed1582001a | | First build with single controller, inputs and outputs not aligned. | n/a
 afi-06740c40be3315e44 | agfi-0f79d721e3edefc64 | master-b86bfd8d65490545b4ace0aab3fbae19bf027652 Single controller with 64b aligned input and output, double buffering | n/a
 afi-064af6a9ebb4349d9 | agfi-0275df76295dbc8c1 | same as above, but with tlast set via C++ | n/a
 afi-005f604b2e786b217 | agfi-0a8eb87970600ea78 | msm-1x-full-precompute-adder | [Copying scalars and points to gmem] 1.78697s, [Doing actual work] 10.8767s
 afi-071f40ea5e182fa8f | agfi-074c9451b3f89d392 | msm-1x-full-precompute-merge-axi-streams | [transferring scalars to gmem] 0.204802s, [Doing FPGA Computation] 10.8336s
 afi-071f40ea5e182fa8f | agfi-0e2c85bf4591270d3 | msm-halve-window-sizes-2 | [transferring scalars to gmem] 0.277229s, [Doing FPGA Computation] 8.10432s
 afi-0df5b1800bfbfdd54 | agfi-036994fb80202cb8d | mega-build-3-oct-1 | [transferring scalars to gmem] 0.182392s, [Doing FPGA Computation] 6.8731s
 afi-066aeb84a7663930a | agfi-0ec73e4a50c84b9fc | mega-build-3-oct-1, various timing optimizations, 250MHz, Vivado 2021.2 | [Doing FPGA Computation] 5.40025s 

# Running `host_buckets.exe`

`host_buckets.exe` is a debug application that pumps test vectors into the
FPGA from a file, and compare against a reference file.

Firstly, compile the host binaries:

```bash
cd fantastic-carnival/zprize/msm_pippenger/host
mkdir build/
cd build/

# Need to explicitly use cmake3 in the aws box, since it's running a pretty old
# centos
cmake3 ..
make -j

# Now, generate the test vectors. This just needs to be done once. Here's an
# example for debugging with 50k points
dune build @../../hardcaml/bin/default
../../hardcaml/bin/tools.exe test-vectors \
  -num-points 50_000 \
  -input-filename inputs-50_000.txt \
  -output-filename outputs-50_000.txt \
  -seed 1

# Now, run the actual binary. It should say "TEST PASSED" in the end. If you
# see verbose test output with things that looks like coordinates, your test
# probably failed.
./driver/host_buckets \
  path/to/msm_pippenger.link.awsxclbin \
  inputs-50000.txt \
  outputs-50000.txt
```

## Accelerating NTT Operations on FPGA


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
