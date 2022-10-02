# FPGA Implementation of the MSM pippengers algorithm

## Optimizations used

* Twisted edwards curve and extended projective points for a lower latency point
  multiplication algorithm
* Allow points to be streamed in batches from the host
* Multiplier optimizations aimed at the Barret reduction algorithm
* Selecting a bucket size that allows for efficent usage of FPGA URAM, and
  pblocking to avoid routing congestion
* Scalars are converted into a +/- form so that bucket memory get a free bit per
  window
* Fully pipelined point adder
* Offloading the final triangle sum to the host
* Using BRAM to store coefficents that can be used to reduce modulo adder and
  subtractor latency

## Block diagrams

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

You need to run these steps on the run box. Make sure you have cloned the
aws-fpga repo and run:

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

# Running `host_buckets.exe` debug test

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

# Benchmarking 2<sup>26</sup> points

See the README.md [here](test_fpga_harness/README.md) for instructions on
benchmarking our solution.

## AFI-ids and measured performance

AFI-id | AFI-gid | Notes | 2^26 performance
------- | ------- | ----- | -----
 afi-04f8603ed1582001a | | First build with single controller, inputs and outputs not aligned. | n/a
 afi-06740c40be3315e44 | agfi-0f79d721e3edefc64 | master-b86bfd8d65490545b4ace0aab3fbae19bf027652 Single controller with 64b aligned input and output, double buffering | n/a
 afi-064af6a9ebb4349d9 | agfi-0275df76295dbc8c1 | same as above, but with tlast set via C++ | n/a
 afi-005f604b2e786b217 | agfi-0a8eb87970600ea78 | msm-1x-full-precompute-adder | [Copying scalars and points to gmem] 1.78697s, [Doing actual work] 10.8767s
 afi-071f40ea5e182fa8f | agfi-074c9451b3f89d392 | msm-1x-full-precompute-merge-axi-streams | [transferring scalars to gmem] 0.204802s, [Doing FPGA Computation] 10.8336s
 afi-071f40ea5e182fa8f | agfi-0e2c85bf4591270d3 | msm-halve-window-sizes-2 | [transferring scalars to gmem] 0.277229s, [Doing FPGA Computation] 8.10432s
