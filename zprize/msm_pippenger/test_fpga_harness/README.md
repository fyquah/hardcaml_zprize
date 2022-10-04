# MSM FPGA Test Harness

This is modified version of the GPU test harness for testing our FPGA msm design. A
feature we added is the ability to load test data from file using an environment
variable `TEST_LOAD_DATA_FROM`.

## A Note about Points Representation

As the original GPU test harness expected the points returned by the FPGA to
be in projective form. We modified our host driver code to convert our result
point to this form.

The input and output points Basefield values are all in (or expected to be in)
montgomery space (montgomery here refers to montgomery multiplication). This
is also some extra work that we are required to do in precomputation and
during evaluation, as we don't represent our points internally in montgomery
space.

## Generating Test Data

_Note: If you have already previously generated test data, you can skip this step._

Run with freshly generated points and save the testdata and save the testdata
to a directory. Note that this overwrites anything you have in 'path/to/write/to'
without warning!

```bash
# The --nocapture flag will cause println! in rust to be displayed to the user
CMAKE=cmake3 XCLBIN=<file> TEST_NPOW=10 TEST_WRITE_DATA_TO=path/to/write/to cargo test --release -- --nocapture
```

For `TEST_NPOW=10`, this should run in a few seconds. For `TEST_NPOW=26`, it can take
around 6-7hours to generate all the points.

## Running Tests

To load the points from disk, use the `TEST_LOAD_DATA_FROM` env-var. Note that
you don't have to specify `TEST_NPOW` when specifying `TEST_LOAD_DATA_FROM`.
(If you do, it will just be ignored)

```bash
# The --nocapture flag will cause println! in rust to be displayed to the user
CMAKE=cmake3 XCLBIN=<file> TEST_LOAD_DATA_FROM=path/to/load/from cargo test --release -- --nocapture
```

Here are the expected runtime for a 2^26 test:
- around 5-10m to load the test data
- around 20-30m to run `multi_scalar_mult_init`, as it needs to convert all the points given to it
  into twisted edwards form.
- around 20s per MSM of batch size 4

For debugging some driver code, it's usually easier to test on a set of trivial
inputs (all points are G1 generator, all scalars are zero except for scalars[0]).
Testdata for 2^26 takes <5s to generate on AWS.

```bash
CMAKE=cmake3 XCLBIN=<file> TEST_TRIVIAL_INPUTS=1 XCLBIN=<file> cargo test --release -- --nocapture
```

To run the tests on the FPGA box with freshly generated points. Note that if
you don't specify `TEST_NPOW`, the test harness will raise (this is different
from the original GPU test harness)

```bash
CMAKE=cmake3 XCLBIN=<file> TEST_NPOW=10 cargo test --release -- --nocapture
```

## Running Benchmarks

This is similar to running tests, except unstead of running `cargo test`, you
run `cargo bench`. The expect environment variables are similar

```bash
CMAKE=cmake3 \
  XCLBIN=<XCLBIN> \
  TEST_LOAD_DATA_FROM=~/testdata/2_16 \
  cargo bench
```

# Note for Zprize judges

In our aws runner box, we have pregenerated a bunch of test cases and save them in `~/testdata`.
We have found that loading them from disk is a lot faster than regenerating everytime for larger
tests (even then, loading 2^26 points from disk on the runner box still took at least
5 minuets!).
