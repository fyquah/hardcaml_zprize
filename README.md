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

After running the compile\_afi.sh script, there should be a folder 'afi/'. Get
the afi id from the file afi/\...\_afi_id.txt this to get the afi id and run:

```
aws ec2 describe-fpga-images --fpga-image-ids afi-06740c40be3315e44
```
Which will show up as "available" when the image is ready to use.


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

## AFI-ids
A place to record AFI image ids and keep track of what point in the code they were made.
AFI-id | AFI-gid | Notes | 2^26 performance
------- | ------- | ----- | -----
 afi-04f8603ed1582001a | | First build with single controller, inputs and outputs not aligned. | n/a
 afi-06740c40be3315e44 | agfi-0f79d721e3edefc64 | master-b86bfd8d65490545b4ace0aab3fbae19bf027652 Single controller with 64b aligned input and output, double buffering | n/a
 afi-064af6a9ebb4349d9 | agfi-0275df76295dbc8c1 | same as above, but with tlast set via C++ | n/a
 afi-005f604b2e786b217 | agfi-0a8eb87970600ea78 | msm-1x-full-precompute-adder | [Copying scalars and points to gmem] 1.78697s, [Doing actual work] 10.8767s
 afi-071f40ea5e182fa8f | agfi-074c9451b3f89d392 | msm-1x-full-precompute-merge-axi-streams | [transferring scalars to gmem] 0.204802s, [Doing FPGA Computation] 10.8336s
 afi-071f40ea5e182fa8f | agfi-0e2c85bf4591270d3 | msm-halve-window-sizes-2 | [transferring scalars to gmem] 0.277229s, [Doing FPGA Computation] 8.10432s

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
