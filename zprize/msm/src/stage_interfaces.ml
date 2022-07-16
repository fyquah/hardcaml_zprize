open Core
module M = Stage_interfaces_intf.M

module Make (Config : Config.S) = struct
  open Config

  let log_datapath_num_entries = Int.ceil_log2 datapath_num_entries
  let log_precomputed_points_table_size = Int.ceil_log2 precomputed_points_table_size

  module Intermediate_point_lookup = struct
    module Stage_input = struct
      type 'a t =
        { entry_index : 'a [@bits log_datapath_num_entries]
        ; precomputed_point_address : 'a [@bits log_precomputed_points_table_size]
        ; is_last : 'a
        ; is_handling_doubling_fault : 'a
        ; is_streaming_result : 'a
        ; is_first : 'a
        ; valid : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end

    module Stage_output = struct
      type 'a t =
        { entry_index : 'a [@bits log_datapath_num_entries]
        ; precomputed_point_address : 'a [@bits log_precomputed_points_table_size]
        ; intermediate_point : 'a Jacobian_point_with_metadata.t
        ; is_last : 'a
        ; is_handling_doubling_fault : 'a
        ; is_streaming_result : 'a
        ; valid : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end
  end

  module Doubling = struct
    module Stage_input = Intermediate_point_lookup.Stage_output

    module Stage_output = struct
      type 'a t =
        { entry_index : 'a [@bits log_datapath_num_entries]
        ; intermediate_point : 'a Jacobian_point_with_metadata.t
        ; (* XXX fyquah: Feels a bit whacky to put this as a separate field. *)
          intermediate_point_z_squared : 'a [@bits 377]
        ; intermediate_point_write_to_buffer : 'a
        ; precomputed_point_address : 'a [@bits log_precomputed_points_table_size]
        ; is_handling_doubling_fault : 'a
        ; valid : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end
  end

  module Precomputed_point_lookup = struct
    module Stage_input = Doubling.Stage_output

    module Stage_output = struct
      type 'a t =
        { entry_index : 'a [@bits log_datapath_num_entries]
        ; intermediate_point : 'a Jacobian_point_with_metadata.t
              [@rtlprefix "intermediate$"]
        ; intermediate_point_z_squared : 'a
              [@bits 377] [@rtlprefix "intermediate$z_squared"]
        ; precomputed_point : 'a Affine_point_or_infinity.t [@rtlprefix "precomputed$"]
        ; valid : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end
  end

  module Adding = struct
    module Stage_input = Precomputed_point_lookup.Stage_output

    module Stage_output = struct
      type 'a t =
        { entry_index : 'a [@bits log_datapath_num_entries]
        ; intermediate_point : 'a Jacobian_point_with_metadata.t
        ; valid : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end
  end

  module Intermediate_point_writeback_stage = struct
    module Stage_output = struct
      type 'a t =
        { entry_index : 'a [@bits log_datapath_num_entries]
        ; intermediate_point : 'a Jacobian_point_with_metadata.t
        ; valid : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end
  end
end
