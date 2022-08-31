## Build Instructions
NOTE: There's probably a bunch of `gmp` specific optimizations we can do (https://gmplib.org/manual/Installing-GMP).

```
mkdir build
cd build
cmake -DCMAKE_BUILD_TYPE=Release ..
make -j
```
