# FPGA Implementation of the MSM pippengers algorithm

We have implemented a FPGA design that runs on a AWS F1 instance and can compute
the MSM of a large number of eliptic point and scalar pairs on the BLS12-377
curve.

## Optimizations used

* Twisted edwards curve and extended projective points for a lower latency point
  addition algorithm, and a high performance fully-pipelined adder on the FPGA
* Mask PCIe latency by allowing MSM operations to start while points are being
  streamed in batches from the host
* Multiplier optimizations aimed at the Barret reduction algorithm and how they
  map to the FPGA
* Selecting a bucket size that allows for efficent usage of FPGA URAM, and
  pblocking to avoid routing congestion
* Scalars are converted into a signed form so that bucket memory gets a free bit
  per window
* Offloading the final triangle sum to the host
* Using BRAM to store coefficents that can be used to reduce modulo adder and
  subtractor latency

## Block diagram

TODO


# Benchmarking 2<sup>26</sup> points

See the README.md [here](test_fpga_harness/README.md) for instructions on
benchmarking our solution.

## AFI-ids and measured performance

We have listed all the AFI-ids and their performance at certain points in the
repo. Currently the highest performance one is:

afi-066aeb84a7663930a

A single 2<sup>26</sup> MSM:
```
[memcpy-ing scalars to special memory region] 0.254459s
[transferring scalars to gmem] 0.269489s
[Doing FPGA Computation] 5.40025s
[Copying results back from gmem] 0.00128308s
[Doing on-host postprocessing] 0.475065s
```

The total time for 4 back to back MSMs, repeated 10 times. This allows us to
mask the overhead of transfering data to the FPGA and various host processing
steps on the scalar inputs that can happen in parallel.
```
FPGA-MSM/2**26x4        time:   [25.480 s 25.500 s 25.520 s]
```
We acheive a mean of 25.500s, which equates to 10.526M MSM op/s.

AWS allows the average power to be measured during operation:
```
Power consumption (Vccint):
   Last measured: 49 watts
   Average: 49 watts
   Max measured: 52 watts
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

### Notes

When running the tests if you terminate the binary early by `ctrl-c`, it will
leave the FPGA in a bad state which requires clearing and re-programming with
these commands:

```
sudo fpga-clear-local-image  -S 0
sudo fpga-load-local-image -S 0 -I <afig-...>

```

# Building for AWS

The AFIs above can be built from source using the following instructions (after
installing OCaml so the Verilog source can be built).

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
aws ec2 describe-fpga-images --fpga-image-ids <afi-...>
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

We have listed all the AFI-ids and their performance at certain points in the
repo. Currently the highest performance one is:

afi-066aeb84a7663930a
```
[memcpy-ing scalars to special memory region] 0.254459s
[transferring scalars to gmem] 0.269489s
[Doing FPGA Computation] 5.40025s
[Copying results back from gmem] 0.00128308s
[Doing on-host postprocessing] 0.475065s
```

When doing a run over 4 batches, which allows us to only pay for the scalar
transfer time of the first batch, we get a total run time of 24.000s, or 11.18M
op/s.

AWS allows the average power to be measured during operation:
```
Power consumption (Vccint):
   Last measured: 49 watts
   Average: 49 watts
   Max measured: 52 watts
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
