open Hardcaml_axi

module Read = struct
  type 'a t =
    { datapath_max_num_entries : 'a [@bits 32]
    ; precomputed_points_table_size : 'a [@bits 32]
    }
  [@@deriving sexp_of, hardcaml]
end

module Write = struct
  type 'a t =
    { num_entries_minus_1 : 'a [@bits 32]
    ; scalar_num_bits_minus_1 : 'a [@bits 32]
    }
  [@@deriving sexp_of, hardcaml]

  let addresses = scan t ~init:0 ~f:(fun acc _ -> acc + 4, acc)
end

module Register_bank = Axi32.Lite.Register_bank (Read) (Write)
