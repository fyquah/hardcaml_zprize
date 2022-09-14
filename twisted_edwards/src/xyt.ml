module Make (Num_bits : Num_bits.S) = struct
  open Num_bits

  type 'a t =
    { x : 'a [@bits num_bits]
    ; y : 'a [@bits num_bits]
    ; t : 'a [@bits num_bits]
    }
  [@@deriving sexp_of, hardcaml]
end
