#include <stdint.h>

#include "ap_axi_sdata.h"
#include "ap_int.h"
#include "hls_stream.h"

#define MEMORY_DWIDTH 512

typedef ap_axiu<MEMORY_DWIDTH, 1, 0, 0> chunk_t;

/* Suppressing loop flattening below prevents DSP usages. We do pay a constant 1-2 cycles exiting and
 * entering the loop, but row_size is 2^12, so this is negligible.
 */

void phase1(
    hls::stream<chunk_t>&   compute_to_controller,
    hls::stream<chunk_t>&   controller_to_compute,
    ap_uint<MEMORY_DWIDTH>* gmem_in,
    ap_uint<MEMORY_DWIDTH>* gmem_out,
    ap_uint<16>             row_size) {
#pragma HLS INLINE off
#pragma HLS dataflow

// dataflow optimization should ensure that phase1_load and phase1_store
// runs in parallel. But this unfortunately means csim will not work.. which
// we don't care about anyway.
//
// flattening is important to ensure that it is sufficiently pipelined.

// TODO(fyquah): Do manual loop flattening to remove DSP usage

  // Phase 1: read transposed, writeback transposed.
phase1_load:
  for (ap_uint<16> i = 0; i < row_size / 8; i++) {
    for (ap_uint<16> j = 0; j < row_size; j++) {
#pragma HLS pipeline II=1
      chunk_t v;

      v.data = gmem_in[i + (j * (row_size / 8))];
      v.user = 0;
      controller_to_compute.write(v);
    }
  }

phase1_store:
  for (ap_uint<16> i = 0; i < row_size / 8; i++) {
    for (ap_uint<16> j = 0; j < row_size; j++) {
#pragma HLS pipeline II=1
      chunk_t v = compute_to_controller.read();
      gmem_out[i + (j * (row_size / 8))] = v.data;
    }
  }
}

void phase2(
    hls::stream<chunk_t>&   compute_to_controller,
    hls::stream<chunk_t>&   controller_to_compute,
    ap_uint<MEMORY_DWIDTH>* gmem_in,
    ap_uint<MEMORY_DWIDTH>* gmem_out,
    ap_uint<16>             row_size)
{
#pragma HLS INLINE off
#pragma HLS dataflow

  // Phase 2: read linear, writeback transposed
phase2_load:
  for (ap_uint<16> i = 0; i < row_size / 8; i++) {
    for (ap_uint<16> j = 0; j < row_size / 8; j++) {
      for (ap_uint<16> k = 0; k < 8; k++) {
#pragma HLS pipeline II=1
        chunk_t v;
        ap_uint<512> d = gmem_in[j + ((8 * i + k) * (row_size / 8))];
        v.data = d;
        v.user = 1;
        controller_to_compute.write(v);
      }
    }
  }

phase2_store:
  for (ap_uint<16> i = 0; i < row_size / 8; i++) {
    for (ap_uint<16> j = 0; j < row_size; j++) {
#pragma HLS pipeline II=1

      chunk_t v = compute_to_controller.read();
      gmem_out[i + (j * (row_size / 8))] = v.data;
    }
  }
}


void krnl_controller(
    hls::stream<chunk_t>&   compute_to_controller,
    hls::stream<chunk_t>&   controller_to_compute_phase_1,
    hls::stream<chunk_t>&   controller_to_compute_phase_2,
    ap_uint<MEMORY_DWIDTH>* gmem_a,
    ap_uint<MEMORY_DWIDTH>* gmem_b,
    ap_uint<16>             row_size,
    ap_uint<2>              phase) {
#pragma HLS INTERFACE m_axi     port = gmem_a offset = slave bundle = gmem_a
#pragma HLS INTERFACE m_axi     port = gmem_b offset = slave bundle = gmem_b
#pragma HLS INTERFACE axis      port = compute_to_controller
#pragma HLS INTERFACE axis      port = controller_to_compute_phase_1
#pragma HLS INTERFACE axis      port = controller_to_compute_phase_2
#pragma HLS INTERFACE s_axilite port = gmem_a
#pragma HLS INTERFACE s_axilite port = gmem_b
#pragma HLS INTERFACE s_axilite port = row_size
#pragma HLS INTERFACE s_axilite port = return

  if (phase(0, 0)) {
    phase1(compute_to_controller, controller_to_compute_phase_1, gmem_a, gmem_b, row_size);
  }

  if (phase(1, 1)) {
    phase2(compute_to_controller, controller_to_compute_phase_2, gmem_b, gmem_a, row_size);
  }
}
