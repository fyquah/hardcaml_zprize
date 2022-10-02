The test harness currently only supports afi-0df5b1800bfbfdd54
(msm-halve-window-sizes-2)

# Running Tests

Our AWS box has some testdata of various sizes stored in `~/testdata`

To load the points from disk, use the `TEST_LOAD_DATA_FROM` env-var. Note that
you don't have to specify `TEST_NPOW`.  (If you do, it will just be ignored)

```bash
# The --nocapture flag will cause println! in rust to be displayed to the user
CMAKE=cmake3 XCLBIN=<file> TEST_LOAD_DATA_FROM=path/to/load/from cargo test --release -- --nocapture
```

- `2^26` takes around 5-10minutes to load (6h to generate), 20-30m to run
  `multi_scalar_mult_init`, as it needs to convert all the points given to it
  into twisted edwards form.
- `2^10` loads points and runs test within 2s

For debugging some driver code, it's usually easier to test on a set of trivial
inputs (all points are G1 generator, all scalars are zero except for scalars[0]).
Testdata for 2^26 takes <5s to generate on AWS.

```bash
CMAKE=cmake3 XCLBIN=<file> TEST_TRIVIAL_INPUTS=1 XCLBIN=<file> cargo test --release -- --nocapture
```

To run the tests on the FPGA box with freshly generated points. Note that if
you don't specify `TEST_NPOW`, the test harness will raise (this is a key
difference from the original GPU test harness)

```bash
CMAKE=cmake3 XCLBIN=<file> TEST_NPOW=10 cargo test --release -- --nocapture
```

# Running Benchmarking

This is similar to running tests, except unstead of running `cargo test`, you
run `cargo bench`

```bash
CMAKE=cmake3 \
  XCLBIN=~/afi-0c117aa4642bded48.awsxclbin \
  TEST_LOAD_DATA_FROM=~/testdata/2_16 \
  cargo bench
```

# Generating Test Data

WARNING: You probably don't need to do the following, be careful not to
overwrite existing testdata!!!

Run with freshly generated points and save the testdata and save the testdata
to a directory. Note that this overwrites anything you have in 'path/to/write/to'
without warning!

```bash
CMAKE=cmake3 XCLBIN=<file> TEST_NPOW=10 TEST_WRITE_DATA_TO=path/to/write/to cargo test --release
```
