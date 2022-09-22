open Hardcaml

module Make (Config : Hardcaml_ntt.Core_config.S) : sig
  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; tready : 'a
      ; start : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O : sig
    type 'a t =
      { done_ : 'a
      ; tvalid : 'a
      ; rd_addr : 'a
      ; rd_en : 'a
      ; block : 'a
      ; first_preroll : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  val create : Scope.t -> Hardcaml.Signal.t Hardcaml.Interface.Create_fn(I)(O).t
  val hierarchy : Scope.t -> Hardcaml.Signal.t Hardcaml.Interface.Create_fn(I)(O).t
end
