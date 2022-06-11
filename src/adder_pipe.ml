open Base
open Hardcaml
open Reg_with_enable

module Make_comb_implementation(Comb : Comb.S) = struct
  open Comb

  type stage_input =
    { input_parts : Comb.t list
    }

  type stage_output =
    { carry : Comb.t option
    ; accumulated_result : Comb.t
    }

  let map_stage_output ~f { carry; accumulated_result } =
    { carry = Option.map ~f carry
    ; accumulated_result = f accumulated_result
    }
  ;;

  let validate_same_width p =
    let w = width (List.hd_exn p) in
    List.iter p ~f:(fun x ->
        assert (width x = w));
    w
  ;;

  let create_stage { carry; accumulated_result } ({ input_parts } : stage_input) =
    let part_width = width (List.hd_exn input_parts) in
    List.iter input_parts ~f:(fun p ->
        assert (width p = part_width));
    let max_adder_result =
      let carry_in_width =
        match carry with
        | None -> 0
        | Some x -> width x
      in
      let open Z in
      (of_int (List.length input_parts) * ((one lsl part_width) - one))
      +  ((one lsl carry_in_width) - one)
    in
    let partial_adder_width = Z.(log2up (max_adder_result + one)) in
    let this_output =
      List.concat [
        (match carry with
         | None -> []
         | Some x -> [ x ])
      ; input_parts
      ]
      |> List.map ~f:(Fn.flip uresize partial_adder_width)
      |> List.reduce_exn ~f:( +: )
    in
    { carry = Some (drop_bottom this_output part_width)
    ; accumulated_result = concat_msb_e [ sel_bottom this_output part_width; accumulated_result ]
    }
  ;;

  let create ~stages ~pipe inputs =
    let bits = validate_same_width inputs in
    let input_parts =
      let part_width = (bits + (stages - 1)) / stages in
      List.map inputs ~f:(split_lsb ~exact:false ~part_width)
      |> List.transpose_exn
    in
    let { accumulated_result; carry = _ } =
      List.foldi input_parts
        ~init:{ carry = None; accumulated_result = empty }
        ~f:(fun n acc input_parts ->
            map_stage_output ~f:(pipe ~n:1)
              (create_stage acc { input_parts = List.map ~f:(pipe ~n) input_parts }))
    in
    accumulated_result
  ;;
end

open Make_comb_implementation(Signal)

module With_interface(M : sig
    val bits : int
    val num_inputs : int
  end) = struct
  include M

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
