open Base
open Hardcaml
open Signal

module Make (Config : Core_config.S) = struct
  open Config

  let cores = 1 lsl logcores

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; start : 'a
      ; first_4step_pass : 'a
      ; first_iter : 'a
      ; flip : 'a
      ; wr_d : 'a array [@bits Gf.Signal.num_bits] [@length cores]
      ; wr_en : 'a [@bits cores]
      ; wr_addr : 'a [@bits logn]
      ; rd_en : 'a [@bits cores]
      ; rd_addr : 'a [@bits logn]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { done_ : 'a
      ; rd_q : 'a array [@bits Gf.Signal.num_bits] [@length cores]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module Single_core = Single_core.With_rams (Config)

  let create_with_multiple_controllers ~start_row ~build_mode scope (i : _ I.t) =
    let cores =
      Array.init cores ~f:(fun index ->
        Single_core.hierarchy
          ~row:(start_row + index)
          ~build_mode
          ~instance:("ntt" ^ Int.to_string index)
          scope
          { Single_core.I.clock = i.clock
          ; clear = i.clear
          ; start = i.start
          ; first_iter = i.first_iter
          ; first_4step_pass = i.first_4step_pass
          ; flip = i.flip
          ; wr_d = i.wr_d.(index)
          ; wr_en = i.wr_en.:(index)
          ; wr_addr = i.wr_addr
          ; rd_en = i.rd_en.:(index)
          ; rd_addr = i.rd_addr
          })
    in
    { O.done_ = cores.(0).done_; rd_q = Array.map cores ~f:(fun core -> core.rd_q) }
  ;;

  module Controller = Controller.Make (Config)
  module Datapath = Datapath.Make (Config)

  let create_with_single_controller ~start_row ~build_mode scope (i : _ I.t) =
    let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let controller =
      Controller.hierarchy
        scope
        { Controller.I.clock = i.clock
        ; clear = i.clear
        ; start = i.start
        ; first_iter = i.first_iter
        ; first_4step_pass = i.first_4step_pass
        }
    in
    let rd_q =
      let pipe ~n = pipeline spec ~n in
      let pipe_dp = pipe ~n:(Core_config.datapath_latency + 1) in
      let read_enable_in = controller.read_write_enable in
      let write_enable_out = controller.read_write_enable |> pipe_dp in
      let addr1_in = controller.addr1 in
      let addr2_in = controller.addr2 in
      let addr1_out = controller.addr1 |> pipe_dp in
      let addr2_out = controller.addr2 |> pipe_dp in
      let flip = controller.flip in
      let last_stage = controller.last_stage in
      Array.init cores ~f:(fun index ->
        let datapath = Datapath.O.Of_signal.wires () in
        let d_in_0, d_in_1 =
          Single_core.input_ram
            scope
            build_mode
            ~clock:i.clock
            ~clear:i.clear
            ~flip:i.flip
            ~wr_addr:i.wr_addr
            ~wr_d:i.wr_d.(index)
            ~wr_en:i.wr_en.:(index)
            ~addr1_in
            ~addr2_in
            ~read_enable_in
        in
        let d_out_0, d_out_1 =
          Single_core.transpose_ram
            scope
            build_mode
            ~clock:i.clock
            ~clear:i.clear
            ~addr1_in
            ~addr1_out
            ~q1:datapath.q1
            ~addr2_in
            ~addr2_out
            ~q2:datapath.q2
            ~read_enable_in
            ~write_enable_out
            ~flip
            ~last_stage
        in
        let first_stage = pipe ~n:1 controller.first_stage in
        let d1 = mux2 first_stage d_in_0 d_out_0 in
        let d2 = mux2 first_stage d_in_1 d_out_1 in
        Datapath.O.Of_signal.assign
          datapath
          (Datapath.hierarchy
             ~row:(start_row + index)
             scope
             { Datapath.I.clock = i.clock
             ; clear = i.clear
             ; start = i.start
             ; first_iter = i.first_iter
             ; d1
             ; d2
             ; omegas = controller.omegas
             ; start_twiddles = controller.start_twiddles
             ; twiddle_stage = controller.twiddle_stage
             ; twiddle_update = controller.twiddle_update
             });
        Single_core.output_ram
          scope
          build_mode
          ~clock:i.clock
          ~clear:i.clear
          ~flip:i.flip
          ~last_stage:(pipe_dp last_stage)
          ~twiddle_stage:(pipe_dp controller.twiddle_stage)
          ~rd_addr:i.rd_addr
          ~rd_en:i.rd_en.:(index)
          ~addr1_out
          ~q1:datapath.q1
          ~addr2_out
          ~q2:datapath.q2
          ~write_enable_out)
    in
    { O.done_ = controller.done_; rd_q }
  ;;

  let create ?(single_controller = true) =
    if single_controller
    then create_with_single_controller
    else create_with_multiple_controllers
  ;;

  let hierarchy ?single_controller ~start_row ~build_mode scope =
    let module Hier = Hierarchy.In_scope (I) (O) in
    Hier.hierarchical
      ~name:"parallel_cores"
      ~scope
      (create ?single_controller ~build_mode ~start_row)
  ;;
end
