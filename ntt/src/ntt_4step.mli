open! Base
open Hardcaml

val logn : int
val logcores : int

module Gf : module type of Gf_bits.Make (Hardcaml.Signal)

module Ntt : module type of Ntt.Make (struct
  let logn = logn
end)

module Parallel_cores : sig
  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; start : 'a
      ; wr_d : 'a
      ; wr_en : 'a
      ; wr_addr : 'a
      ; rd_en : 'a
      ; rd_addr : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O : sig
    type 'a t =
      { done_ : 'a
      ; rd_q : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  val create : Scope.t -> Signal.t Interface.Create_fn(I)(O).t
end

module Twiddle_controller : sig
  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; start : 'a
      ; d : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O : sig
    type 'a t =
      { done_ : 'a
      ; addr : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  val create : Scope.t -> Signal.t Interface.Create_fn(I)(O).t
end

module Controller : sig
  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; start : 'a
      ; cores_done : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O : sig
    type 'a t =
      { done_ : 'a
      ; start_cores : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  val create : Scope.t -> Signal.t Interface.Create_fn(I)(O).t
end

module Core : sig
  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; start : 'a
      ; wr_d : 'a
      ; wr_en : 'a
      ; wr_addr : 'a
      ; rd_en : 'a
      ; rd_addr : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O : sig
    type 'a t =
      { done_ : 'a
      ; rd_q : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  val create : Scope.t -> Signal.t Interface.Create_fn(I)(O).t
end
