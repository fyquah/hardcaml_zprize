module type S = sig
  val datapath_num_entries : int
  val precomputed_points_table_size : int
  val precomputed_points_table_read_latency : int
  val intermediate_points_table_read_latency : int
end

module type Config = sig
  module type S = S

  module Big : S
end
