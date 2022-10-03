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

![Block diagram](docs/block_diagram.png)

## Resource utilization
```
+----------------------------+--------+--------+--------+--------+--------+--------+
|          Site Type         |  SLR0  |  SLR1  |  SLR2  | SLR0 % | SLR1 % | SLR2 % |
+----------------------------+--------+--------+--------+--------+--------+--------+
| CLB                        |  25089 |  39672 |  38105 |  50.93 |  80.54 |  77.35 |
|   CLBL                     |  12592 |  19723 |  18557 |  51.19 |  80.17 |  75.43 |
|   CLBM                     |  12497 |  19949 |  19548 |  50.68 |  80.90 |  79.27 |
| CLB LUTs                   | 109566 | 133493 | 146117 |  27.80 |  33.87 |  37.08 |
|   LUT as Logic             | 102296 | 118359 | 125869 |  25.96 |  30.03 |  31.94 |
|     using O5 output only   |    962 |   1351 |     13 |   0.24 |   0.34 |  <0.01 |
|     using O6 output only   |  80045 |  76731 |  79799 |  20.31 |  19.47 |  20.25 |
|     using O5 and O6        |  21289 |  40277 |  46057 |   5.40 |  10.22 |  11.69 |
|   LUT as Memory            |   7270 |  15134 |  20248 |   3.69 |   7.67 |  10.26 |
|     LUT as Distributed RAM |   7002 |   4268 |      0 |   3.55 |   2.16 |   0.00 |
|     LUT as Shift Register  |    268 |  10866 |  20248 |   0.14 |   5.51 |  10.26 |
|       using O5 output only |      0 |      0 |      0 |   0.00 |   0.00 |   0.00 |
|       using O6 output only |     96 |   4782 |   8248 |   0.05 |   2.42 |   4.18 |
|       using O5 and O6      |    172 |   6084 |  12000 |   0.09 |   3.08 |   6.08 |
| CLB Registers              | 159643 | 281789 | 294290 |  20.26 |  35.75 |  37.34 |
| CARRY8                     |   1518 |   7589 |  18131 |   3.08 |  15.41 |  36.81 |
| F7 Muxes                   |   4818 |   1251 |      0 |   2.45 |   0.63 |   0.00 |
| F8 Muxes                   |    279 |    226 |      0 |   0.28 |   0.23 |   0.00 |
| F9 Muxes                   |      0 |      0 |      0 |   0.00 |   0.00 |   0.00 |
| Block RAM Tile             |  153.5 |    239 |     28 |  21.32 |  33.19 |   3.89 |
|   RAMB36/FIFO              |    152 |    235 |     24 |  21.11 |  32.64 |   3.33 |
|     RAMB36E2 only          |    128 |    235 |     24 |  17.78 |  32.64 |   3.33 |
|   RAMB18                   |      3 |      8 |      8 |   0.21 |   0.56 |   0.56 |
|     RAMB18E2 only          |      3 |      8 |      8 |   0.21 |   0.56 |   0.56 |
| URAM                       |    210 |    127 |    126 |  65.63 |  39.69 |  39.38 |
| DSPs                       |      0 |    859 |   2140 |   0.00 |  37.68 |  93.86 |
| Unique Control Sets        |   4089 |   4425 |    119 |   4.15 |   4.49 |   0.12 |
+----------------------------+--------+--------+--------+--------+--------+--------+
```

# Benchmarking 2<sup>26</sup> points

See the README.md [here](test_fpga_harness/README.md) for instructions on
benchmarking our solution.

## AFI-ids and measured performance

We have listed all the AFI-ids and their performance at certain points in the
repo. Currently the highest performance afi is:

afi-0b83061a1938e28cb (FPGA MSM kernel running at 270MHz)

Note these tests take up to 30min each as we transform 2<sup>26</sup> affine
points into their twisted edwards representation.

Running `cargo test` to verify the result for 4 rounds of 2<sup>26</sup> MSM:

```
Running MSM of [67108864] input points (4 batches)
Streaming input scalars across 4 chunks per batch (Mask IO and Post Processing)
Running multi_scalar_mult took Ok(20.957301742s) (round = 0)
test msm_correctness ... ok
```

The total time for 4 back to back MSMs, repeated 10 times (the output of cargo
bench in the [test\_fpga\_harness](test_fpga_harness/README.md)). This allows us
to mask the overhead of transfering data to the FPGA and various host processing
steps on the scalar inputs that can happen in parallel. This is also the
required measurement outlined in the ZPrize specs.

```
FPGA-MSM/2**26x4        time:  [20.915 s 20.915 s 20.916 s]
```

We acheive a mean of 20.915s, which equates to **12.835** Mop/s
((4*2^26)/1000000)/29.915).

AWS allows the average power to be measured during operation:
```
sudo fpga-describe-local-image -S 0 -M
```
```
Power consumption (Vccint):
   Last measured: 51 watts
   Average: 50 watts
   Max measured: 54 watts
```

#### Note
Because our solution offloads a non-trival amount of work to perform in parallel
to the host, you will see the best performance after a fresh reboot, and without
other CPU-intensive tasks running at the same time.

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
