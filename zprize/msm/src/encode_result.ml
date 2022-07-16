open Base
open Hardcaml
open Hardcaml_axi
open Signal

module Make (Config : Config.S) = struct
  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; result : 'a Jacobian_point_or_infinity.With_valid.t
      ; result_last : 'a
      ; result_to_host_dest : 'a Axi128.Stream.Dest.t
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { result_to_host : 'a Axi128.Stream.Source.t
      ; result_ready : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create
      (scope : Scope.t)
      { I.clock; clear; result; result_last; result_to_host_dest }
    =
    let spec = Reg_spec.create ~clock ~clear () in
    let rd_fifo = wire 1 in
    let fifo =
      Jacobian_point.iter result.value.point ~f:(fun a -> assert (width a = 377));
      let { Jacobian_point_or_infinity.point = { x; y; z }; is_infinity } =
        result.value
      in
      let x = uresize x 384 in
      let y = uresize y 384 in
      Fifo.create
        ~scope:(Scope.sub_scope scope "encoded_fifo")
        ~showahead:true
        ~capacity:8
        ~clock
        ~clear
        ~wr:result.valid
        ~d:
          (result_last @: uresize (is_infinity @: z) 384 @: uresize y 384 @: uresize x 384)
        ~rd:rd_fifo
        ()
    in
    let fifo_q_last = msb fifo.q in
    let fifo_q = Signal.split_lsb ~part_width:128 (lsbs fifo.q) in
    let expected_num_parts = 9 in
    assert (List.length fifo_q = expected_num_parts);
    let tready = result_to_host_dest.tready in
    let tvalid = ~:(fifo.empty) &: ~:clear in
    let pos =
      reg_fb
        spec
        ~width:(Int.ceil_log2 expected_num_parts)
        ~enable:(tvalid &: tready)
        ~f:(fun fb -> mux2 (fb ==:. expected_num_parts - 1) (zero (width fb)) (fb +:. 1))
    in
    let last_part_of_word = pos ==:. expected_num_parts - 1 in
    rd_fifo <== (result_to_host_dest.tready &: ~:(fifo.empty) &: last_part_of_word);
    { O.result_to_host =
        { tvalid
        ; tdata = mux pos fifo_q
        ; tkeep = ones 16
        ; tstrb = ones 16
        ; tlast = fifo_q_last &: last_part_of_word
        }
    ; result_ready = ~:(fifo.full)
    }
  ;;

  let hierarchical scope i =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~name:"encode_result" ~scope create i
  ;;
end
