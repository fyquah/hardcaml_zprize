#include <stdint.h>

#include "ap_axi_sdata.h"
#include "ap_int.h"
#include "hls_stream.h"

#define MEMORY_DWIDTH 512

typedef ap_axiu<MEMORY_DWIDTH, 0, 0, 0> chunk_t;

void demultiplex_stream(
    hls::stream<chunk_t>& src,
    hls::stream<ap_uint<512>>& dst_a,
    hls::stream<ap_uint<512>>& dst_b,
    ap_uint<16> arg_row_size) {
#pragma HLS INLINE off
  ap_uint<16> row_size = arg_row_size;
  for (ap_uint<16> i = 0; i < row_size / 8; i++) {
#pragma HLS pipeline off

demux_a_loop_body:
    for (ap_uint<16> j = 0; j < row_size; j++) {
#pragma HLS pipeline II=1
      chunk_t x = src.read();
      dst_a.write(x.data);
    }

demux_b_loop_body:
    for (ap_uint<16> j = 0; j < row_size; j++) {
#pragma HLS pipeline II=1
      chunk_t x = src.read();
      dst_b.write(x.data);
    }
  }
}

void multiplex_stream(
    hls::stream<ap_uint<512>>& src_a,
    hls::stream<ap_uint<512>>& src_b,
    hls::stream<chunk_t>& dst,
    ap_uint<16> arg_row_size) {
#pragma HLS INLINE off
  ap_uint<16> row_size = arg_row_size;
  for (ap_uint<16> i = 0; i < row_size / 8; i++) {
#pragma HLS pipeline off

mux_loop_a_body:
    for (ap_uint<16> j = 0; j < row_size; j++) {
#pragma HLS pipeline II=1
      chunk_t chunk;
      ap_uint<512> data;

      data = src_a.read();
      chunk.data = data;
      dst.write(chunk);
    }

mux_loop_b_body:
    for (ap_uint<16> j = 0; j < row_size; j++) {
#pragma HLS pipeline II=1
      chunk_t chunk;
      ap_uint<512> data;

      data = src_b.read();
      chunk.data = data;
      dst.write(chunk);
    }
  }
}

void worker(
    hls::stream<ap_uint<512>>& src,
    hls::stream<ap_uint<512>>& dst,
    ap_uint<16>              arg_row_size) {
#pragma HLS INLINE off
  ap_uint<16> row_size = arg_row_size;
  ap_uint<64> buf0[4096];
  ap_uint<64> buf1[4096];
  ap_uint<64> buf2[4096];
  ap_uint<64> buf3[4096];
  ap_uint<64> buf4[4096];
  ap_uint<64> buf5[4096];
  ap_uint<64> buf6[4096];
  ap_uint<64> buf7[4096];

  for (uint16_t i = 0; i < row_size / 8; i++) {
    for (uint16_t j = 0; j < row_size; j++) {
      ap_uint<512> data = src.read();
  
      buf0[j] = data(63 ,   0);
      buf1[j] = data(127,  64);
      buf2[j] = data(191, 128);
      buf3[j] = data(255, 192);
      buf4[j] = data(319, 256);
      buf5[j] = data(383, 320);
      buf6[j] = data(447, 384);
      buf7[j] = data(511, 448);
    }
  
    for (uint16_t j = 0; j < row_size; j++) {
      ap_uint<64> d0 = buf0[row_size - 1 - j];
      ap_uint<64> d1 = buf1[row_size - 1 - j];
      ap_uint<64> d2 = buf2[row_size - 1 - j];
      ap_uint<64> d3 = buf3[row_size - 1 - j];
      ap_uint<64> d4 = buf4[row_size - 1 - j];
      ap_uint<64> d5 = buf5[row_size - 1 - j];
      ap_uint<64> d6 = buf6[row_size - 1 - j];
      ap_uint<64> d7 = buf7[row_size - 1 - j];
      ap_uint<512> data = (d7, d6, d5, d4, d3, d2, d1, d0);

      dst.write(data);
    }
  }
}

void krnl_reverse(
    hls::stream<chunk_t>& compute_to_controller,
    hls::stream<chunk_t>& controller_to_compute,
    uint16_t              row_size) {
#pragma HLS INTERFACE axis      port = compute_to_controller
#pragma HLS INTERFACE axis      port = controller_to_compute
#pragma HLS INTERFACE s_axilite port = row_size
#pragma HLS INTERFACE s_axilite port = return

    hls::stream<ap_uint<512>> controller_to_compute_a;
    hls::stream<ap_uint<512>> controller_to_compute_b;
    hls::stream<ap_uint<512>> compute_to_controller_a;
    hls::stream<ap_uint<512>> compute_to_controller_b;
#pragma HLS STREAM variable=controller_to_compute_a depth=2
#pragma HLS STREAM variable=controller_to_compute_b depth=2
#pragma HLS STREAM variable=compute_to_controller_a depth=2
#pragma HLS STREAM variable=compute_to_controller_b depth=2

#pragma HLS dataflow
    demultiplex_stream(controller_to_compute, controller_to_compute_a, controller_to_compute_b, row_size);
    worker(controller_to_compute_a, compute_to_controller_a, row_size);
    worker(controller_to_compute_b, compute_to_controller_b, row_size);
    multiplex_stream(compute_to_controller_a, compute_to_controller_b, compute_to_controller, row_size);

}
