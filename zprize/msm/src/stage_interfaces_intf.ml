module M (Config : Config.S) = struct
  module type S = sig
    module Intermediate_point_lookup : sig
      module Stage_input : sig
        type 'a t =
          { entry_index : 'a
          ; precomputed_point_address : 'a
          ; is_last : 'a
          ; is_handling_doubling_fault : 'a
          ; is_streaming_result : 'a
          ; is_first : 'a
          ; valid : 'a
          }
        [@@deriving sexp_of, hardcaml]
      end

      module Stage_output : sig
        type 'a t =
          { entry_index : 'a
          ; precomputed_point_address : 'a
          ; intermediate_point : 'a Jacobian_point_with_metadata.t
          ; is_last : 'a
          ; is_handling_doubling_fault : 'a
          ; is_streaming_result : 'a
          ; valid : 'a
          }
        [@@deriving sexp_of, hardcaml]
      end
    end

    module Doubling : sig
      module Stage_input = Intermediate_point_lookup.Stage_output

      module Stage_output : sig
        type 'a t =
          { entry_index : 'a
          ; intermediate_point : 'a Jacobian_point_with_metadata.t
          ; intermediate_point_z_squared : 'a
          ; intermediate_point_write_to_buffer : 'a
          ; precomputed_point_address : 'a
          ; is_handling_doubling_fault : 'a
          ; valid : 'a
          }
        [@@deriving sexp_of, hardcaml]
      end
    end

    module Precomputed_point_lookup : sig
      module Stage_input = Doubling.Stage_output

      module Stage_output : sig
        type 'a t =
          { entry_index : 'a
          ; intermediate_point : 'a Jacobian_point_with_metadata.t
          ; intermediate_point_z_squared : 'a
          ; precomputed_point : 'a Affine_point_or_infinity.t
          ; valid : 'a
          }
        [@@deriving sexp_of, hardcaml]
      end
    end

    module Adding : sig
      module Stage_input = Precomputed_point_lookup.Stage_output

      module Stage_output : sig
        type 'a t =
          { entry_index : 'a
          ; intermediate_point : 'a Jacobian_point_with_metadata.t
          ; valid : 'a
          }
        [@@deriving sexp_of, hardcaml]
      end
    end

    module Intermediate_point_writeback_stage : sig
      module Stage_output : sig
        type 'a t =
          { entry_index : 'a
          ; intermediate_point : 'a Jacobian_point_with_metadata.t
          ; valid : 'a
          }
        [@@deriving sexp_of, hardcaml]
      end
    end
  end
end

module type Stage_interfaces = sig
  module M = M
  module Make (Config : Config.S) : M(Config).S
end
