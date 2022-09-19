open Hardcaml

module type S = sig
  module Source : sig
    type 'a t =
      { tvalid : 'a
      ; tdata : 'a
      ; tkeep : 'a
      ; tstrb : 'a
      ; tlast : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module Dest : sig
    type 'a t = { tready : 'a } [@@deriving sexp_of, hardcaml]
  end

  module Register : sig
    module I : sig
      type 'a t =
        { clock : 'a
        ; clear : 'a
        ; up : 'a Source.t
        ; dn_dest : 'a Dest.t
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O : sig
      type 'a t =
        { dn : 'a Source.t
        ; up_dest : 'a Dest.t
        }
      [@@deriving sexp_of, hardcaml]
    end

    val create : Scope.t -> Signal.t I.t -> Signal.t O.t
    val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t
  end
end

module type Stream = sig
  module type S = S

  module Make (_ : Config.S) : S
end
