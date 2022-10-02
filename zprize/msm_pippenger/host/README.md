## Build Instructions
NOTE: There's probably a bunch of `gmp` specific optimizations we can do (https://gmplib.org/manual/Installing-GMP).

```
source ~/aws-fpga/vitis_setup.sh
mkdir build
cd build
cmake -DCMAKE_BUILD_TYPE=Release ..
make -j
```

### clang-format
```
find driver naive_driver msm_compute -iname *.h -o -iname *.cpp | xargs clang-format --style=Google -i
```
