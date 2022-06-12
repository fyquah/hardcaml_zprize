open Base
open Hardcaml
open Reg_with_enable

type ('result, 'carries) result =
  { result  : 'result
  ; carries : 'carries
  }

module Implementation(Comb : Comb.S) = struct
  open Comb

  type stage_input =
    { input_parts : Comb.t list
    }

  type borrow_and_partial_result =
    { carries        : t list
    ; partial_result : t
    }

  let map_stage_output ~f { carries; result } =
    { carries = List.map ~f carries
    ; result = f result
    }
  ;;

  let validate_same_width p =
    let w = width (List.hd_exn p) in
    List.iter p ~f:(fun x ->
        assert (width x = w));
    w
  ;;

  let rec create_stage_impl ~op ~carry_ins ~input_parts =
    match input_parts, carry_ins with
    | [ partial_result ], [] ->
      { partial_result; carries = [] }
    | (a :: b :: tl_parts), (carry_in :: tl_carry_in) ->
      let this_sum =
        let w = width a in
        op
          (op (uresize a (w + 1)) (uresize b (w + 1)))
          (uresize carry_in (w + 1))
      in
      let this_carry = msb this_sum in
      let { partial_result; carries = remaining_carries } =
        create_stage_impl
          ~op
          ~input_parts:(lsbs this_sum :: tl_parts)
          ~carry_ins:tl_carry_in
      in
      { partial_result
      ; carries = this_carry :: remaining_carries
      }
    | _, _ -> assert false
  ;;

  let create_stage
      ~op
      { carries = carry_ins; result }
      ({ input_parts } : stage_input) =
    let part_width = width (List.hd_exn input_parts) in
    List.iter input_parts ~f:(fun p ->
        assert (width p = part_width));
    assert (List.length carry_ins = List.length input_parts - 1);
    let { partial_result; carries } =
      create_stage_impl
        ~op:(match op with | `Add -> ( +: ) | `Sub -> ( -: ))
        ~carry_ins
        ~input_parts
    in
    { result = concat_msb_e [ partial_result; result ]
    ; carries
    }
  ;;

  let create ~op ~stages ~pipe inputs =
    let bits = validate_same_width inputs in
    let input_parts =
      (* [ a; b; c ] -> [ [ a1; b1; c1 ]; [ a2; b2; c2; ] [ a3; b3; c3 ] ]
       *
       * where a = concat_lsb [ a1; a2; a3 ]
       *)
      let part_width = (bits + (stages - 1)) / stages in
      List.map inputs ~f:(split_lsb ~exact:false ~part_width)
      |> List.transpose_exn
    in
    let num_parts = List.length (List.hd_exn input_parts) in
    List.foldi input_parts
      ~init:{ carries = List.init (num_parts - 1) ~f:(Fn.const gnd)
            ; result  = empty
            }
      ~f:(fun n acc input_parts ->
          map_stage_output ~f:(pipe ~n:1)
            (create_stage ~op acc { input_parts = List.map ~f:(pipe ~n) input_parts }))
  ;;
end

module With_interface(M : sig
    val bits : int
    val num_inputs : int
  end) = struct
  include M
  include Implementation(Signal)

  module I = struct
    type 'a t =
      { clock : 'a
      ; enable : 'a
      ; data : 'a list [@bits bits] [@length num_inputs] [@rtlprefix "in_"]
      ; valid : 'a [@rtlprefix "in_"]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { result : 'a [@bits bits] [@rtlprefix "out_"]
      ; valid : 'a [@rtlprefix "out_"]
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create ~op ~stages (_ : Scope.t) ({ I. clock; enable; data; valid } : _ I.t) =
    let spec = Reg_spec.create ~clock () in
    let pipe ~n x = pipeline spec ~n ~enable x in
    let { result; carries = _ } = create ~op ~stages ~pipe data in
    let valid = pipe ~n:stages valid in
    { O. result; valid }
  ;;

  let hierarchical ~op ~stages scope i =
    let module H = Hierarchy.In_scope(I)(O) in
    H.hierarchical
      ~scope
      ~name:(Printf.sprintf
               "%s_pipe_%dbits_%dinputs"
               (match op with
                | `Add -> "add"
                | `Sub -> "sub")
               bits
               num_inputs)
      (create ~op ~stages)
      i
  ;;
end

let hierarchical ~scope ~clock ~enable ~op ~stages data =
  let bits = Signal.width (List.hd_exn data) in
  let module M =
    With_interface(struct
      let num_inputs = List.length data
      let bits = bits
    end)
  in
  let { M.O. result; valid = _ } =
    M.hierarchical ~op ~stages scope { clock; enable; data; valid = Signal.vdd }
  in
  result
;;

module For_testing = struct
  let create_combinational (type a)
      (module Comb : Comb.S with type t = a)
      ~op
      ~stages
      inputs =
    let open Implementation(Comb) in
    create ~op ~stages ~pipe:(fun ~n:_ x -> x) inputs
  ;;
end
