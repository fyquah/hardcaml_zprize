module Make (Num_bits : Num_bits.S) : sig
  type 'a t =
    { x : 'a
    ; y : 'a
    ; z : 'a
    ; t : 'a
    }
  [@@deriving sexp_of, hardcaml]
end
