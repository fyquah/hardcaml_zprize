open! Base
open! Hardcaml
module Gf : module type of Gf_bits.Make (Signal)

module Controller : sig
  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; start : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O : sig
    type 'a t =
      { done_ : 'a
      ; i : 'a
      ; j : 'a
      ; k : 'a
      ; m : 'a
      ; addr1 : 'a
      ; addr2 : 'a
      ; omega : 'a
      ; start_twiddles : 'a
      ; first_stage : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  val create : Scope.t -> Signal.t Interface.Create_fn(I)(O).t
  val hierarchy : Scope.t -> Signal.t Interface.Create_fn(I)(O).t
end

module Datapath : sig
  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; d1 : 'a
      ; d2 : 'a
      ; omega : 'a
      ; start_twiddles : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O : sig
    type 'a t =
      { q1 : 'a
      ; q2 : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  val create : Scope.t -> Signal.t Interface.Create_fn(I)(O).t
  val hierarchy : Scope.t -> Signal.t Interface.Create_fn(I)(O).t
end

module Core : sig
  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; start : 'a
      ; d1 : 'a
      ; d2 : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O : sig
    type 'a t =
      { q1 : 'a
      ; q2 : 'a
      ; addr1_in : 'a [@bits logn]
      ; addr2_in : 'a [@bits logn]
      ; read_enable_in : 'a
      ; addr1_out : 'a [@bits logn]
      ; addr2_out : 'a [@bits logn]
      ; write_enable_out : 'a
      ; first_stage : 'a
      ; done_ : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  val create : Scope.t -> Signal.t Interface.Create_fn(I)(O).t
  val hierarchy : Scope.t -> Signal.t Interface.Create_fn(I)(O).t
end

module With_rams : sig
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
  val hierarchy : Scope.t -> Signal.t Interface.Create_fn(I)(O).t
end

module Reference : sig
  module Gf : module type of Gf_bits.Make (Hardcaml.Bits)

  val bit_reversed_addressing : 'a array -> unit

  (** From the reference implementation provided to the competition *)
  val ntt : Gf.t array -> unit

  (** Standard decimation in time algorithm *)
  val dit : Gf.t array -> unit

  (** Standard decimation in frequency algorithm *)
  val dif : Gf.t array -> unit

  val matrix : Gf.t array -> int -> int -> Gf.t array array
  val transpose : Gf.t array array -> Gf.t array array
  val row : Gf.t array array -> int -> Gf.t array
  val col : Gf.t array array -> int -> Gf.t array

  (** Four step decomposition *)
  val four_step : Gf.t array -> int -> Gf.t array
end
