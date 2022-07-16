open Hardcaml_axi

module Read : sig
  type 'a t =
    { datapath_max_num_entries : 'a
    ; precomputed_points_table_size : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module Write : sig
  type 'a t =
    { num_entries_minus_1 : 'a
    ; scalar_num_bits_minus_1 : 'a
    }
  [@@deriving sexp_of, hardcaml]

  val addresses : int t
end

module Register_bank : module type of Axi32.Lite.Register_bank (Read) (Write)
