# Compiling the Reference

Run `cargo build` in `rust/ark_bls12_377_g1` to compile the dynamic library
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
You might need to install the “m4” package. In ubuntu, you can
run
```
sudo apt-get install m4
```

To check everything installed correctly
```
opam exec -- dune build
opam exec -- dune runtest
```


# Building for AWS

You need to clone the aws-fpga repo: https://github.com/aws/aws-fpga/

```
source ~/aws-fpga/vitis_setup.sh
source ~/aws-fpga/vitis_runtime_setup.sh
```

If you want the Vivado GUI over the ssh to AWS, you need to install:
```
yum install libXtst.x86_64
```

Building from scratch:
```
cd zprize/msm_pippenger/fpga
dune build
./compile_hw.sh or ./compile_hw_emu.sh
```

Testing:
Modify xrt.template.ini if you want to disable GUI.
```
cd zprize/msm_pippenger/test
./run_hw_emu.sh
```

Creating the AWS AFI:
```
cd zprize/msm_pippenger/fpga
./compile_afi.sh
```
