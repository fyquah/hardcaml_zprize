#include <stdint.h>

#include "ap_axi_sdata.h"
#include "ap_int.h"
#include "hls_stream.h"

#define MEMORY_DWIDTH 512

typedef ap_axiu<MEMORY_DWIDTH, 0, 0, 0> chunk_t;

/* Bit manipulation API detailed here:
 * https://docs.xilinx.com/r/2020.2-English/ug1399-vitis-hls/Other-Class-Methods-Operators-and-Data-Members
 *
 * Unfortunately, we cannot inline this. Experimentally, the HLS toolchain performs CSE on different instances
 * of extract_and_concat and shares the "shifting hardware", which means the extracts below gets implemented
 * as really expensive shifts (250k LUTS vs 5k LUTS).
 *
 * XXX fyquah: Check if there is a pragma that can suppress the behaviour above.
 */
template<uint64_t lo>
static chunk_t
extract_and_concat(const ap_uint<512 * 8> d) {
#pragma HLS latency min=0 max=0
#pragma HLS INLINE OFF
  const uint64_t hi = lo + 63;
  ap_uint<64> x7 = d(7 * 512 + hi, 7 * 512 + lo);
  ap_uint<64> x6 = d(6 * 512 + hi, 6 * 512 + lo);
  ap_uint<64> x5 = d(5 * 512 + hi, 5 * 512 + lo);
  ap_uint<64> x4 = d(4 * 512 + hi, 4 * 512 + lo);
  ap_uint<64> x3 = d(3 * 512 + hi, 3 * 512 + lo);
  ap_uint<64> x2 = d(2 * 512 + hi, 2 * 512 + lo);
  ap_uint<64> x1 = d(1 * 512 + hi, 1 * 512 + lo);
  ap_uint<64> x0 = d(0 * 512 + hi, 0 * 512 + lo);

  ap_uint<512> data = (x7, x6, x5, x4, x3, x2, x1, x0);

  chunk_t chunk;
  chunk.data = data;

  return chunk;
}

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

// TODO(fyquah): This uses 8 DSPs. Fix it (it should be trivial)
ap_uint<512*8> load_words_linearly(
    ap_uint<MEMORY_DWIDTH>* gmem_in,
    ap_uint<16> i,
    ap_uint<16> j,
    ap_uint<16> row_size)
{
#pragma HLS INLINE off
  ap_uint<512> d0 = gmem_in[j + ((8 * i + 0) * (row_size / 8))];
  ap_uint<512> d1 = gmem_in[j + ((8 * i + 1) * (row_size / 8))];
  ap_uint<512> d2 = gmem_in[j + ((8 * i + 2) * (row_size / 8))];
  ap_uint<512> d3 = gmem_in[j + ((8 * i + 3) * (row_size / 8))];
  ap_uint<512> d4 = gmem_in[j + ((8 * i + 4) * (row_size / 8))];
  ap_uint<512> d5 = gmem_in[j + ((8 * i + 5) * (row_size / 8))];
  ap_uint<512> d6 = gmem_in[j + ((8 * i + 6) * (row_size / 8))];
  ap_uint<512> d7 = gmem_in[j + ((8 * i + 7) * (row_size / 8))];
  return (d7, d6, d5, d4, d3, d2, d1, d0);
}

void stream_words_lienarly(
    hls::stream<chunk_t>& controller_to_compute,
    ap_uint<512*8> d)
{
#pragma HLS INLINE off
  controller_to_compute.write(extract_and_concat<0>  (d));
  controller_to_compute.write(extract_and_concat<64> (d));
  controller_to_compute.write(extract_and_concat<128>(d));
  controller_to_compute.write(extract_and_concat<192>(d));
  controller_to_compute.write(extract_and_concat<256>(d));
  controller_to_compute.write(extract_and_concat<320>(d));
  controller_to_compute.write(extract_and_concat<384>(d));
  controller_to_compute.write(extract_and_concat<448>(d));
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
#pragma HLS pipeline II=1

      auto d = load_words_linearly(gmem_in, i, j, row_size);
      stream_words_lienarly(controller_to_compute, d);
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
    hls::stream<chunk_t>&   controller_to_compute,
    ap_uint<MEMORY_DWIDTH>* gmem_a,
    ap_uint<MEMORY_DWIDTH>* gmem_b,
    ap_uint<16>             row_size) {
#pragma HLS INTERFACE m_axi     port = gmem_a offset = slave bundle = gmem_a
#pragma HLS INTERFACE m_axi     port = gmem_b offset = slave bundle = gmem_b
#pragma HLS INTERFACE axis      port = compute_to_controller
#pragma HLS INTERFACE axis      port = controller_to_compute
#pragma HLS INTERFACE s_axilite port = gmem_a
#pragma HLS INTERFACE s_axilite port = gmem_b
#pragma HLS INTERFACE s_axilite port = row_size
#pragma HLS INTERFACE s_axilite port = return

  phase1(compute_to_controller, controller_to_compute, gmem_a, gmem_b, row_size);
  phase2(compute_to_controller, controller_to_compute, gmem_b, gmem_a, row_size);
}
