open Base
open Hardcaml
open Reg_with_enable

module Adder_implementation(Comb : Comb.S) = struct
  open Comb

  type stage_input =
    { input_parts : Comb.t list
    }

  type stage_output =
    { carries : Comb.t list option
    ; accumulated_result : Comb.t
    }

  type carry_and_partial_sum =
    { carries : Comb.t list
    ; partial_sum : Comb.t
    }

  let map_stage_output ~f { carries; accumulated_result } =
    { carries = Option.map ~f:(List.map ~f) carries
    ; accumulated_result = f accumulated_result
    }
  ;;

  let validate_same_width p =
    let w = width (List.hd_exn p) in
    List.iter p ~f:(fun x ->
        assert (width x = w));
    w
  ;;

  (* The desired arthiecture we want to generate in every stage is
   *
   * > LUT > CARRY8 > LUT > CARRY8
   *           ^              ^
   * > LUT > CARRY8 > LUT > CARRY8
   *           ^              ^
   * > LUT > CARRY8 > LUT > CARRY8
   *           ^              ^
   * > LUT > CARRY8 > LUT > CARRY8
   *
   * This uses the least resources (ignoring FFs..) with a modest critical path
   * of N - 1 LUTs and the carry chain for summing up N number.
  *)
  let rec create_stage_impl ~(carry_ins : Comb.t option list) ~input_parts =
    match input_parts, carry_ins with
    | [ partial_sum ], [] ->
      { partial_sum; carries = [] }
    | (a :: b :: tl_parts), (carry_in :: tl_carry_in) ->
      let this_sum =
        let w = width a in
        match carry_in with
        | None -> Uop.(a +: b)
        | Some carry_in -> Uop.(a +: b) +: uresize carry_in (w + 1)
      in
      let this_carry = msb this_sum in
      let { partial_sum; carries = remaining_carries } =
        create_stage_impl
          ~input_parts:(lsbs this_sum :: tl_parts)
          ~carry_ins:tl_carry_in
      in
      { partial_sum
      ; carries = this_carry :: remaining_carries
      }
    | _, _ -> assert false
  ;;

  let create_stage
      { carries = carry_ins; accumulated_result }
      ({ input_parts } : stage_input) =
    let part_width = width (List.hd_exn input_parts) in
    List.iter input_parts ~f:(fun p ->
        assert (width p = part_width));
    let carry_ins =
      match carry_ins with
      | None ->
        List.init ~f:(Fn.const None) (List.length input_parts - 1)
      | Some l ->
        assert (List.length l = List.length input_parts - 1);
        List.map l ~f:(fun x -> Some x)
    in
    let { partial_sum; carries } = create_stage_impl ~carry_ins ~input_parts in
    { accumulated_result = concat_msb_e [ partial_sum; accumulated_result ]
    ; carries = Some carries
    }
  ;;

  let create ~stages ~pipe inputs =
    let bits = validate_same_width inputs in
    let input_parts =
      let part_width = (bits + (stages - 1)) / stages in
      List.map inputs ~f:(split_lsb ~exact:false ~part_width)
      |> List.transpose_exn
    in
    let { accumulated_result; carries = _ } =
      List.foldi input_parts
        ~init:{ carries = None; accumulated_result = empty }
        ~f:(fun n acc input_parts ->
            map_stage_output ~f:(pipe ~n:1)
              (create_stage acc { input_parts = List.map ~f:(pipe ~n) input_parts }))
    in
    accumulated_result
  ;;
end

module With_interface(M : sig
    val bits : int
    val num_inputs : int
  end) = struct
  include M
  include Adder_implementation(Signal)

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
      { sum : 'a [@bits bits] [@rtlprefix "out_"]
      ; valid : 'a [@rtlprefix "out_"]
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create ~stages (_ : Scope.t) ({ I. clock; enable; data; valid } : _ I.t) =
    let spec = Reg_spec.create ~clock () in
    let pipe ~n x = pipeline spec ~n ~enable x in
    let sum = create ~stages ~pipe data in
    let valid = pipe ~n:stages valid in
    { O. sum; valid }
  ;;

  let hierarchical ~stages scope i =
    let module H = Hierarchy.In_scope(I)(O) in
    H.hierarchical
      ~scope
      ~name:(Printf.sprintf "adder_pipe_%dbits_%dinputs" bits num_inputs)
      (create ~stages)
      i
  ;;
end

let hierarchical ~scope ~clock ~enable ~stages data =
  let bits = Signal.width (List.hd_exn data) in
  let module M =
    With_interface(struct
      let num_inputs = List.length data
      let bits = bits
    end)
  in
  let { M.O. sum; valid = _ } =
    M.hierarchical ~stages scope { clock; enable; data; valid = Signal.vdd }
  in
  sum
;;

module For_testing = struct
  let create_combinational (type a)
      (module Comb : Comb.S with type t = a)
      ~stages
      inputs =
    let open Adder_implementation(Comb) in
    create ~stages ~pipe:(fun ~n:_ x -> x) inputs
  ;;
end
