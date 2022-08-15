open! Base
open Hardcaml

module Make (Size : sig
  val logn : int
end) : sig
  val logcores : int

  module Gf : module type of Gf_bits.Make (Hardcaml.Signal)

  module Parallel_cores : sig
    module I : sig
      type 'a t =
        { clock : 'a
        ; clear : 'a
        ; start : 'a
        ; flip : 'a
        ; wr_d : 'a array
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
        ; rd_q : 'a array
        }
      [@@deriving sexp_of, hardcaml]
    end

    val create
      :  build_mode:Build_mode.t
      -> Scope.t
      -> Signal.t Interface.Create_fn(I)(O).t

    val hierarchy
      :  build_mode:Build_mode.t
      -> Scope.t
      -> Signal.t Interface.Create_fn(I)(O).t
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
    val hierarchy : Scope.t -> Signal.t Interface.Create_fn(I)(O).t
  end

  module Controller : sig
    module I : sig
      type 'a t =
        { clock : 'a
        ; clear : 'a
        ; start : 'a
        ; input_done : 'a
        ; output_done : 'a
        ; cores_done : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O : sig
      type 'a t =
        { done_ : 'a
        ; start_input : 'a
        ; start_output : 'a
        ; start_cores : 'a
        ; flip : 'a
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
        ; wr_d : 'a array
        ; wr_en : 'a
        ; wr_addr : 'a
        ; rd_en : 'a
        ; rd_addr : 'a
        ; input_done : 'a
        ; output_done : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O : sig
      type 'a t =
        { done_ : 'a
        ; start_input : 'a
        ; start_output : 'a
        ; rd_q : 'a array
        }
      [@@deriving sexp_of, hardcaml]
    end

    val create
      :  build_mode:Build_mode.t
      -> Scope.t
      -> Signal.t Interface.Create_fn(I)(O).t

    val hierarchy
      :  build_mode:Build_mode.t
      -> Scope.t
      -> Signal.t Interface.Create_fn(I)(O).t
  end

  module Kernel : sig
    module Axi512 = Hardcaml_axi.Axi512

    module I : sig
      type 'a t =
        { clock : 'a
        ; clear : 'a
        ; start : 'a
        ; data_in : 'a Axi512.Stream.Source.t
        ; data_out_dest : 'a Axi512.Stream.Dest.t
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O : sig
      type 'a t =
        { data_out : 'a Axi512.Stream.Source.t
        ; data_in_dest : 'a Axi512.Stream.Dest.t
        ; done_ : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end

    module Store_sm : sig
      type 'a t =
        { done_ : 'a
        ; tvalid : 'a
        ; rd_addr : 'a
        ; rd_en : 'a
        }
      [@@deriving sexp_of, hardcaml]

      val create : Signal.t I.t -> start:Signal.t -> Signal.t t
    end

    val create
      :  build_mode:Build_mode.t
      -> Scope.t
      -> Signal.t Interface.Create_fn(I)(O).t

    val hierarchy
      :  build_mode:Build_mode.t
      -> Scope.t
      -> Signal.t Interface.Create_fn(I)(O).t
  end
end
