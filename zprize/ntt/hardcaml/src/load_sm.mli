open Hardcaml

module Make (Config : Top_config.S) : sig
  val write_pipelining : int

  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; first_4step_pass : 'a
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

  val create : Scope.t -> Hardcaml.Signal.t Hardcaml.Interface.Create_fn(I)(O).t
  val hierarchy : Scope.t -> Hardcaml.Signal.t Hardcaml.Interface.Create_fn(I)(O).t
end
