#ifndef HOST_TO_FPGA_TRANSPOSE_H
#define HOST_TO_FPGA_TRANSPOSE_H

#include <stdint.h>

void __attribute__ ((noinline))
host_to_fpga_transpose(uint64_t *arg_dst, const uint64_t *arg_in, uint64_t row_size);

#endif
