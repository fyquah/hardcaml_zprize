open Base
open Hardcaml
open Signal
(* Multipass NTT algorithm using the 4 step method.*)

(* Initially, for simplicity, we are going to do "square matrices". Both steps
   across rows and columns of the input will perform ntts of the same size.

   That's not so unreasonable, as we intend to do 2^12 base ntts to perform the
   full 2^24 ntt anyway. It's not so hard to fix later should we need to (we
   might!).
*)

(* We might also just use a single core to start with. But frankly we must deal
   with mutliple cores (as much as memory can support!) so we need to keep this
   in mind.
*)

(* Overall, we are going to compute an ntt of 2^(logn + logn) *)
let logn = 4

(* Specifying this as a log is probably not as flexible as we want (if we can
   fit 29 cores, this would limit us to 16). But it greatly simplifies things to
   start with. *)
let logcores = 2
let cores = 1 lsl logcores

module Gf = Gf_bits.Make (Hardcaml.Signal)

module Ntt = Ntt.Make (struct
  let logn = logn
end)

module Parallel_cores = struct
  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; start : 'a
      ; wr_d : 'a [@bits Gf.num_bits]
      ; wr_en : 'a
      ; wr_addr : 'a [@bits logn + logcores]
      ; rd_en : 'a
      ; rd_addr : 'a [@bits logn + logcores]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { done_ : 'a
      ; rd_q : 'a [@bits Gf.num_bits]
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create scope (i : _ I.t) =
    let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let cores =
      List.init cores ~f:(fun index ->
          let wr_en = i.wr_addr.:[logcores + logn - 1, logn] ==:. index &: i.wr_en in
          let wr_addr = i.wr_addr.:[logn - 1, 0] in
          let rd_en = i.rd_addr.:[logcores + logn - 1, logn] ==:. index &: i.rd_en in
          let rd_addr = i.rd_addr.:[logn - 1, 0] in
          Ntt.With_rams.hierarchy
            scope
            { Ntt.With_rams.I.clock = i.clock
            ; clear = i.clear
            ; start = i.start
            ; wr_d = i.wr_d
            ; wr_en
            ; wr_addr
            ; rd_en
            ; rd_addr
            })
    in
    { O.done_ = (List.nth_exn cores 0).done_
    ; rd_q =
        mux
          (reg spec i.rd_addr.:[logcores + logn - 1, logn])
          (List.map cores ~f:(fun core -> core.rd_q))
    }
  ;;
end

module Twiddle_controller = struct
  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; start : 'a
      ; d : 'a [@bits Gf.num_bits]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { done_ : 'a
      ; addr : 'a [@bits logn]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module State = struct
    type t =
      | Idle
      | Twiddle_loop
    [@@deriving compare, enumerate, sexp_of, variants]
  end

  module Var = Always.Variable

  let create _scope (i : _ I.t) =
    let open Signal in
    let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let sm = Always.State_machine.create (module State) spec in
    let j = Var.reg spec ~width:logn in
    let j_next = j.value +:. 1 in
    Always.(
      compile
        [ sm.switch
            [ Idle, [ j <--. 0; when_ i.start [ sm.set_next Twiddle_loop ] ]
            ; Twiddle_loop, [ j <-- j_next; when_ (j_next ==:. 0) [ sm.set_next Idle ] ]
            ]
        ]);
    { O.done_ = sm.is Idle; addr = j.value }
  ;;
end

module Controller = struct
  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; start : 'a
      ; cores_done : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { done_ : 'a
      ; start_cores : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module State = struct
    type t =
      | Start
      | Loop
    [@@deriving sexp_of, compare, enumerate]
  end

  module Var = Always.Variable

  let create scope (i : _ I.t) =
    let ( -- ) = Scope.naming scope in
    let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let sm = Always.State_machine.create (module State) spec in
    let start_cores = Var.wire ~default:gnd in
    ignore (sm.current -- "STATE");
    ignore (start_cores.value -- "START_CORES");
    Always.(
      compile
        [ sm.switch
            [ Start, [ when_ i.start [ start_cores <-- vdd; sm.set_next Loop ] ]
            ; Loop, []
            ]
        ]);
    { O.done_ = sm.is Start; start_cores = start_cores.value }
  ;;
end

module Core = struct
  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; start : 'a
      ; wr_d : 'a [@bits Gf.num_bits]
      ; wr_en : 'a
      ; wr_addr : 'a [@bits logn + logcores]
      ; rd_en : 'a
      ; rd_addr : 'a [@bits logn + logcores]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { done_ : 'a
      ; rd_q : 'a [@bits Gf.num_bits]
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create scope (i : _ I.t) =
    let cores_done = wire 1 in
    let controller =
      Controller.create
        scope
        { Controller.I.clock = i.clock; clear = i.clear; start = i.start; cores_done }
    in
    (* let twiddler = Twiddle_controller.create {} *)
    let cores =
      Parallel_cores.create
        scope
        { Parallel_cores.I.clock = i.clock
        ; clear = i.clear
        ; start = controller.start_cores
        ; wr_d = i.wr_d
        ; wr_en = i.wr_en
        ; wr_addr = i.wr_addr
        ; rd_en = i.rd_en
        ; rd_addr = i.rd_addr
        }
    in
    cores_done <== cores.done_;
    { O.done_ = controller.done_; rd_q = cores.rd_q }
  ;;
end
