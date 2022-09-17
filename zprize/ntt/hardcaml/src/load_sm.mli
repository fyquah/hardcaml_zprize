module Make (Config : Hardcaml_ntt.Ntt.Config) : sig
  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; tvalid : 'a
      ; start : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O : sig
    type 'a t =
      { done_ : 'a
      ; tready : 'a
      ; wr_addr : 'a
      ; wr_en : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  val create : Hardcaml.Signal.t Hardcaml.Interface.Create_fn(I)(O).t
end
