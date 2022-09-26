To run the tests on the FPGA box with freshly generated points:

```
CMAKE=cmake3 XCLBIN=<file> TEST_NPOW=10 cargo test --release
```

Run with freshly generated points and save the testdata and save the testdata
to a directory

```
CMAKE=cmake3 XCLBIN=<file> TEST_NPOW=10 TEST_WRITE_DATA_TO=path/to/write/to cargo test --release
```

To load the points from disk. Note that you don't have to specify `TEST_NPOW`:

```
CMAKE=cmake3 XCLBIN=<file> TEST_LOAD_DATA_FROM=path/to/load/from cargo test --release
```
