#include "caml/memory.h"
#include "caml/mlvalues.h"

#include "setup_stream.h"

value caml_msm_setup_precomputed_points_stream_byte(value address,
                                                    value num_points, value x,
                                                    value y, value dst_base) {
  return Val_long(msm_setup_precomputed_points_stream(
      Long_val(address), Long_val(num_points), (const uint64_t *)x,
      (const uint64_t *)y, (uint64_t *)dst_base));
}

value caml_msm_setup_scalars_stream_byte(value num_scalars,
                                         value num_scalars_per_batch,
                                         value num_bits, value scalar_ptr_base,
                                         value dst) {
  return Val_long(msm_setup_scalars_stream(
      Long_val(num_scalars), Long_val(num_scalars_per_batch),
      Long_val(num_bits), (const uint64_t *)scalar_ptr_base, (uint16_t *)dst));
}
