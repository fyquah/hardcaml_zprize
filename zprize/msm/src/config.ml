module type S = Config_intf.S

(* XXX fyquah: Tune these numbers. *)
module Big : S = struct
  let datapath_num_entries = 8192
  let precomputed_points_table_size = 65536
  let precomputed_points_table_read_latency = 3
  let intermediate_points_table_read_latency = 3
end
