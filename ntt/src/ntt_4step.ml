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

module Controller = struct
  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; start : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module Dma_request = struct
    type 'a t =
      { address : 'a
      ; length : 'a
      ; load_or_store : 'a
      ; req : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t = { d : 'a } [@@deriving sexp_of, hardcaml]
  end

  module State = struct
    type t =
      | Start
      | Loop
    [@@deriving sexp_of, compare, enumerate]
  end

  let create scope (i : _ I.t) =
    let ( -- ) = Scope.naming scope in
    let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let sm = Always.State_machine.create (module State) spec in
    let _dma_request = Dma_request.Of_always.reg spec in
    ignore (sm.current -- "STATE");
    Always.(
      compile [ sm.switch [ Start, [ when_ i.start [ sm.set_next Loop ] ]; Loop, [] ] ]);
    O.Of_signal.of_int 0
  ;;
end
