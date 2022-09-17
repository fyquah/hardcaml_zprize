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
static ap_uint<512>
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

  return data;
}

template<uint64_t lo>
static chunk_t
extract_and_concat_chunk(const ap_uint<512 * 8> d) {
  chunk_t v;
  v.data = extract_and_concat<lo>(d);
  return v;
}

/* Suppressing loop flattening below prevents DSP usages. We do pay a constant 1-2 cycles exiting and
 * entering the loop, but row_size is 2^12, so this is negligible.
 */

template<typename T>
T mux8(
    ap_uint<3> sel,
    T x0,
    T x1,
    T x2,
    T x3,
    T x4,
    T x5,
    T x6,
    T x7)
{
  if (sel == 0)
    return x0;

  if (sel == 1)
    return x1;

  if (sel == 2)
    return x2;

  if (sel == 3)
    return x3;

  if (sel == 4)
    return x4;

  if (sel == 5)
    return x5;

  if (sel == 6)
    return x6;

  if (sel == 7)
    return x7;

  return x7;
}

static void
store_to_parallel_channels(
    hls::stream<chunk_t>&   compute_to_controller,
    ap_uint<24> index,
    ap_uint<MEMORY_DWIDTH>* gmem_out0,
    ap_uint<MEMORY_DWIDTH>* gmem_out1,
    ap_uint<MEMORY_DWIDTH>* gmem_out2,
    ap_uint<MEMORY_DWIDTH>* gmem_out3,
    ap_uint<MEMORY_DWIDTH>* gmem_out4,
    ap_uint<MEMORY_DWIDTH>* gmem_out5,
    ap_uint<MEMORY_DWIDTH>* gmem_out6,
    ap_uint<MEMORY_DWIDTH>* gmem_out7
)
{
  for (int k = 0; k < 8; k++) {
    chunk_t v = compute_to_controller.read();
    switch (k) {
    case 0:
      gmem_out0[index] = v.data;
      break;
    case 1:
      gmem_out1[index] = v.data;
      break;
    case 2:
      gmem_out2[index] = v.data;
      break;
    case 3:
      gmem_out3[index] = v.data;
      break;
    case 4:
      gmem_out4[index] = v.data;
      break;
    case 5:
      gmem_out5[index] = v.data;
      break;
    case 6:
      gmem_out6[index] = v.data;
      break;
    case 7:
      gmem_out7[index] = v.data;
      break;
    }
  }
}

void phase1(
    hls::stream<chunk_t>&   compute_to_controller,
    hls::stream<chunk_t>&   controller_to_compute,
    ap_uint<MEMORY_DWIDTH>* gmem_in0,
    ap_uint<MEMORY_DWIDTH>* gmem_in1,
    ap_uint<MEMORY_DWIDTH>* gmem_in2,
    ap_uint<MEMORY_DWIDTH>* gmem_in3,
    ap_uint<MEMORY_DWIDTH>* gmem_in4,
    ap_uint<MEMORY_DWIDTH>* gmem_in5,
    ap_uint<MEMORY_DWIDTH>* gmem_in6,
    ap_uint<MEMORY_DWIDTH>* gmem_in7,
    ap_uint<MEMORY_DWIDTH>* gmem_out0,
    ap_uint<MEMORY_DWIDTH>* gmem_out1,
    ap_uint<MEMORY_DWIDTH>* gmem_out2,
    ap_uint<MEMORY_DWIDTH>* gmem_out3,
    ap_uint<MEMORY_DWIDTH>* gmem_out4,
    ap_uint<MEMORY_DWIDTH>* gmem_out5,
    ap_uint<MEMORY_DWIDTH>* gmem_out6,
    ap_uint<MEMORY_DWIDTH>* gmem_out7,
    ap_uint<16>             row_size) {
#pragma HLS INLINE off
#pragma HLS dataflow

// dataflow optimization should ensure that phase1_load and phase1_store
// runs in parallel. But this unfortunately means csim will not work.. which
// we don't care about anyway.
//
// flattening is important to ensure that it is sufficiently pipelined.

// TODO(fyquah): Do manual loop flattening to remove DSP usage

// TODO(fyquah): Check if awkward case statement actually makes timing better?

  // Phase 1: read transposed, writeback transposed.
phase1_load:
  for (ap_uint<16> i = 0; i < row_size / 8; i++) {
    for (ap_uint<16> j = 0; j < row_size / 8; j++) {
        ap_uint<512> d0 = gmem_in0[i + (j * (row_size / 8))];
        ap_uint<512> d1 = gmem_in1[i + (j * (row_size / 8))];
        ap_uint<512> d2 = gmem_in2[i + (j * (row_size / 8))];
        ap_uint<512> d3 = gmem_in3[i + (j * (row_size / 8))];
        ap_uint<512> d4 = gmem_in4[i + (j * (row_size / 8))];
        ap_uint<512> d5 = gmem_in5[i + (j * (row_size / 8))];
        ap_uint<512> d6 = gmem_in6[i + (j * (row_size / 8))];
        ap_uint<512> d7 = gmem_in7[i + (j * (row_size / 8))];

        for (int k = 0; k < 8; k++) {
          chunk_t v;

          v.data = mux8(k, d0, d1, d2, d3, d4, d5, d6, d7);
          controller_to_compute.write(v);
        }
        // chunk_t v;
        // v.data = gmem_in0[i + (j * (row_size / 8))];
        // controller_to_compute.write(v);
        // v.data = gmem_in1[i + (j * (row_size / 8))];
        // controller_to_compute.write(v);
        // v.data = gmem_in2[i + (j * (row_size / 8))];
        // controller_to_compute.write(v);
        // v.data = gmem_in3[i + (j * (row_size / 8))];
        // controller_to_compute.write(v);
        // v.data = gmem_in4[i + (j * (row_size / 8))];
        // controller_to_compute.write(v);
        // v.data = gmem_in5[i + (j * (row_size / 8))];
        // controller_to_compute.write(v);
        // v.data = gmem_in6[i + (j * (row_size / 8))];
        // controller_to_compute.write(v);
        // v.data = gmem_in7[i + (j * (row_size / 8))];
        // controller_to_compute.write(v);
    }
  }

phase1_store:
  for (ap_uint<16> i = 0; i < row_size / 8; i++) {
    for (ap_uint<16> j = 0; j < row_size / 8; j++) {
      store_to_parallel_channels(
          compute_to_controller,
          i + (j * (row_size / 8)),
          gmem_out0,
          gmem_out1,
          gmem_out2,
          gmem_out3,
          gmem_out4,
          gmem_out5,
          gmem_out6,
          gmem_out7);
    }
  }
}

// TODO(fyquah): This uses 8 DSPs. Fix it (it should be trivial)
ap_uint<512*8> load_words_linearly(
    ap_uint<MEMORY_DWIDTH>* gmem_in0,
    ap_uint<MEMORY_DWIDTH>* gmem_in1,
    ap_uint<MEMORY_DWIDTH>* gmem_in2,
    ap_uint<MEMORY_DWIDTH>* gmem_in3,
    ap_uint<MEMORY_DWIDTH>* gmem_in4,
    ap_uint<MEMORY_DWIDTH>* gmem_in5,
    ap_uint<MEMORY_DWIDTH>* gmem_in6,
    ap_uint<MEMORY_DWIDTH>* gmem_in7,
    ap_uint<16> i,
    ap_uint<16> j,
    ap_uint<16> row_size)
{
#pragma HLS INLINE off

  ap_uint<512> d0 = gmem_in0[j + ((i) * (row_size / 8))];
  ap_uint<512> d1 = gmem_in1[j + ((i) * (row_size / 8))];
  ap_uint<512> d2 = gmem_in2[j + ((i) * (row_size / 8))];
  ap_uint<512> d3 = gmem_in3[j + ((i) * (row_size / 8))];
  ap_uint<512> d4 = gmem_in4[j + ((i) * (row_size / 8))];
  ap_uint<512> d5 = gmem_in5[j + ((i) * (row_size / 8))];
  ap_uint<512> d6 = gmem_in6[j + ((i) * (row_size / 8))];
  ap_uint<512> d7 = gmem_in7[j + ((i) * (row_size / 8))];
  return (d7, d6, d5, d4, d3, d2, d1, d0);
}

void stream_words_lienarly(
    hls::stream<chunk_t>& controller_to_compute,
    ap_uint<512*8> d)
{
#pragma HLS INLINE off
  // controller_to_compute.write(extract_and_concat_chunk<0>  (d));
  // controller_to_compute.write(extract_and_concat_chunk<64> (d));
  // controller_to_compute.write(extract_and_concat_chunk<128>(d));
  // controller_to_compute.write(extract_and_concat_chunk<192>(d));
  // controller_to_compute.write(extract_and_concat_chunk<256>(d));
  // controller_to_compute.write(extract_and_concat_chunk<320>(d));
  // controller_to_compute.write(extract_and_concat_chunk<384>(d));
  // controller_to_compute.write(extract_and_concat_chunk<448>(d));
  auto d0 = extract_and_concat<0> (d);
  auto d1 = extract_and_concat<64>(d);
  auto d2 = extract_and_concat<128>(d);
  auto d3 = extract_and_concat<192>(d);
  auto d4 = extract_and_concat<256>(d);
  auto d5 = extract_and_concat<320>(d);
  auto d6 = extract_and_concat<384>(d);
  auto d7 = extract_and_concat<448>(d);
  for (int i = 0; i < 8; i++) {
    chunk_t chunk;
    chunk.data = mux8(i, d0, d1, d2, d3, d4, d5, d6, d7);
    controller_to_compute.write(chunk);
  }
}

void phase2(
    hls::stream<chunk_t>&   compute_to_controller,
    hls::stream<chunk_t>&   controller_to_compute,
    ap_uint<MEMORY_DWIDTH>* gmem_in0,
    ap_uint<MEMORY_DWIDTH>* gmem_in1,
    ap_uint<MEMORY_DWIDTH>* gmem_in2,
    ap_uint<MEMORY_DWIDTH>* gmem_in3,
    ap_uint<MEMORY_DWIDTH>* gmem_in4,
    ap_uint<MEMORY_DWIDTH>* gmem_in5,
    ap_uint<MEMORY_DWIDTH>* gmem_in6,
    ap_uint<MEMORY_DWIDTH>* gmem_in7,
    ap_uint<MEMORY_DWIDTH>* gmem_out0,
    ap_uint<MEMORY_DWIDTH>* gmem_out1,
    ap_uint<MEMORY_DWIDTH>* gmem_out2,
    ap_uint<MEMORY_DWIDTH>* gmem_out3,
    ap_uint<MEMORY_DWIDTH>* gmem_out4,
    ap_uint<MEMORY_DWIDTH>* gmem_out5,
    ap_uint<MEMORY_DWIDTH>* gmem_out6,
    ap_uint<MEMORY_DWIDTH>* gmem_out7,
    ap_uint<16>             row_size)
{
#pragma HLS INLINE off
#pragma HLS dataflow

  // Phase 2: read linear, writeback transposed
phase2_load:
  for (ap_uint<16> i = 0; i < row_size / 8; i++) {
    for (ap_uint<16> j = 0; j < row_size / 8; j++) {
#pragma HLS pipeline II=1

      auto d = load_words_linearly(
                      gmem_in0, gmem_in1, gmem_in2, gmem_in3, gmem_in4, gmem_in5, gmem_in6, gmem_in7,
                      i, j, row_size);
      stream_words_lienarly(controller_to_compute, d);
    }
  }

phase2_store:
  for (ap_uint<16> i = 0; i < row_size / 8; i++) {
    for (ap_uint<16> j = 0; j < row_size / 8; j++) {
      store_to_parallel_channels(
          compute_to_controller,
          i + (j * (row_size / 8)),
          gmem_out0,
          gmem_out1,
          gmem_out2,
          gmem_out3,
          gmem_out4,
          gmem_out5,
          gmem_out6,
          gmem_out7);
    }
  }
}


void krnl_controller(
    hls::stream<chunk_t>&   compute_to_controller,
    hls::stream<chunk_t>&   controller_to_compute,
    ap_uint<MEMORY_DWIDTH>* gmem0,
    ap_uint<MEMORY_DWIDTH>* gmem1,
    ap_uint<MEMORY_DWIDTH>* gmem2,
    ap_uint<MEMORY_DWIDTH>* gmem3,
    ap_uint<MEMORY_DWIDTH>* gmem4,
    ap_uint<MEMORY_DWIDTH>* gmem5,
    ap_uint<MEMORY_DWIDTH>* gmem6,
    ap_uint<MEMORY_DWIDTH>* gmem7,
    ap_uint<MEMORY_DWIDTH>* gmem8,
    ap_uint<MEMORY_DWIDTH>* gmem9,
    ap_uint<MEMORY_DWIDTH>* gmem10,
    ap_uint<MEMORY_DWIDTH>* gmem11,
    ap_uint<MEMORY_DWIDTH>* gmem12,
    ap_uint<MEMORY_DWIDTH>* gmem13,
    ap_uint<MEMORY_DWIDTH>* gmem14,
    ap_uint<MEMORY_DWIDTH>* gmem15,
    ap_uint<16>             row_size) {
#pragma HLS INTERFACE m_axi     port = gmem0 offset = slave bundle = gmem0
#pragma HLS INTERFACE m_axi     port = gmem1 offset = slave bundle = gmem1
#pragma HLS INTERFACE m_axi     port = gmem2 offset = slave bundle = gmem2
#pragma HLS INTERFACE m_axi     port = gmem3 offset = slave bundle = gmem3
#pragma HLS INTERFACE m_axi     port = gmem4 offset = slave bundle = gmem4
#pragma HLS INTERFACE m_axi     port = gmem5 offset = slave bundle = gmem5
#pragma HLS INTERFACE m_axi     port = gmem6 offset = slave bundle = gmem6
#pragma HLS INTERFACE m_axi     port = gmem7 offset = slave bundle = gmem7
#pragma HLS INTERFACE m_axi     port = gmem8 offset = slave bundle = gmem8
#pragma HLS INTERFACE m_axi     port = gmem9 offset = slave bundle = gmem9
#pragma HLS INTERFACE m_axi     port = gmem10 offset = slave bundle = gmem10
#pragma HLS INTERFACE m_axi     port = gmem11 offset = slave bundle = gmem11
#pragma HLS INTERFACE m_axi     port = gmem12 offset = slave bundle = gmem12
#pragma HLS INTERFACE m_axi     port = gmem13 offset = slave bundle = gmem13
#pragma HLS INTERFACE m_axi     port = gmem14 offset = slave bundle = gmem14
#pragma HLS INTERFACE m_axi     port = gmem15 offset = slave bundle = gmem15
#pragma HLS INTERFACE axis      port = compute_to_controller
#pragma HLS INTERFACE axis      port = controller_to_compute
#pragma HLS INTERFACE s_axilite port = gmem0
#pragma HLS INTERFACE s_axilite port = gmem1
#pragma HLS INTERFACE s_axilite port = gmem2
#pragma HLS INTERFACE s_axilite port = gmem3
#pragma HLS INTERFACE s_axilite port = gmem4
#pragma HLS INTERFACE s_axilite port = gmem5
#pragma HLS INTERFACE s_axilite port = gmem6
#pragma HLS INTERFACE s_axilite port = gmem7
#pragma HLS INTERFACE s_axilite port = gmem8
#pragma HLS INTERFACE s_axilite port = gmem9
#pragma HLS INTERFACE s_axilite port = gmem10
#pragma HLS INTERFACE s_axilite port = gmem11
#pragma HLS INTERFACE s_axilite port = gmem12
#pragma HLS INTERFACE s_axilite port = gmem13
#pragma HLS INTERFACE s_axilite port = gmem14
#pragma HLS INTERFACE s_axilite port = gmem15
#pragma HLS INTERFACE s_axilite port = row_size
#pragma HLS INTERFACE s_axilite port = return

#pragma HLS pipeline off

  phase1(
      /* streams */
      compute_to_controller, controller_to_compute,

      /* gmem_in */
      gmem0, gmem1, gmem2, gmem3, gmem4, gmem5, gmem6, gmem7,

      /* gem_out */
      gmem8, gmem9, gmem10, gmem11, gmem12, gmem13, gmem14, gmem15,

      row_size);
  phase2(
      /* streams */
      compute_to_controller, controller_to_compute,

      /* gem_in */
      gmem8, gmem9, gmem10, gmem11, gmem12, gmem13, gmem14, gmem15,

      /* gmem_out */
      gmem0, gmem1, gmem2, gmem3, gmem4, gmem5, gmem6, gmem7,

      row_size);
}
