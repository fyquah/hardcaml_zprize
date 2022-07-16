external msm_setup_precomputed_points_stream
  :  address:(int[@untagged])
  -> num_points:(int[@untagged])
  -> x:bytes
  -> y:bytes
  -> dst:bytes
  -> (int[@untagged])
  = "caml_msm_setup_precomputed_points_stream_byte" "msm_setup_precomputed_points_stream"
  [@@noalloc]

external msm_setup_scalars_stream
  :  num_scalars:(int[@untagged])
  -> num_scalars_per_batch:(int[@untagged])
  -> num_bits:(int[@untagged])
  -> scalars_buffer:Scalar_buffer.t
  -> dst:bytes
  -> (int[@untagged])
  = "caml_msm_setup_scalars_stream_byte" "msm_setup_scalars_stream"
  [@@noalloc]
