open Base
open Hardcaml
open Reg_with_enable

type ('difference, 'borrows) result =
  { difference : 'difference
  ; borrows    : 'borrows
  }

module Implementation(Comb : Comb.S) = struct
  open Comb

  type stage_input =
    { input_parts : Comb.t list
    }

  type borrow_and_partial_sum =
    { borrows : t list
    ; partial_sum : t
    }

  let map_stage_output ~f { borrows; difference } =
    { borrows = List.map ~f borrows
    ; difference = f difference
    }
  ;;

  let validate_same_width p =
    let w = width (List.hd_exn p) in
    List.iter p ~f:(fun x ->
        assert (width x = w));
    w
  ;;

  let rec create_stage_impl ~(borrow_ins : t list) ~input_parts =
    match input_parts, borrow_ins with
    | [ partial_sum ], [] ->
      { partial_sum; borrows = [] }
    | (a :: b :: tl_parts), (borrow_in :: tl_borrow_in) ->
      let this_sum =
        let w = width a in
        Uop.(a -: b) -: uresize borrow_in (w + 1)
      in
      let this_borrow = msb this_sum in
      let { partial_sum; borrows = remaining_borrows } =
        create_stage_impl
          ~input_parts:(lsbs this_sum :: tl_parts)
          ~borrow_ins:tl_borrow_in
      in
      { partial_sum
      ; borrows = this_borrow :: remaining_borrows
      }
    | _, _ -> assert false
  ;;

  let create_stage
      { borrows = borrow_ins; difference }
      ({ input_parts } : stage_input) =
    let part_width = width (List.hd_exn input_parts) in
    List.iter input_parts ~f:(fun p ->
        assert (width p = part_width));
    let { partial_sum; borrows } = create_stage_impl ~borrow_ins ~input_parts in
    { difference = concat_msb_e [ partial_sum; difference ]
    ; borrows
    }
  ;;

  let create ~stages ~pipe inputs =
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
      ~init:{ borrows = List.init (num_parts - 1) ~f:(Fn.const gnd)
            ; difference = empty
            }
      ~f:(fun n acc input_parts ->
          map_stage_output ~f:(pipe ~n:1)
            (create_stage acc { input_parts = List.map ~f:(pipe ~n) input_parts }))
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
      { difference : 'a [@bits bits] [@rtlprefix "out_"]
      ; borrows : 'a list [@bits 1] [@length num_inputs - 1]
      ; valid : 'a [@rtlprefix "out_"]
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create ~stages (_ : Scope.t) ({ I. clock; enable; data; valid } : _ I.t) =
    let spec = Reg_spec.create ~clock () in
    let pipe ~n x = pipeline spec ~n ~enable x in
    let { difference; borrows } = create ~stages ~pipe data in
    let valid = pipe ~n:stages valid in
    { O. difference; valid; borrows }
  ;;

  let hierarchical ~stages scope i =
    let module H = Hierarchy.In_scope(I)(O) in
    H.hierarchical
      ~scope
      ~name:(Printf.sprintf "subtractor_pipe_%dbits_%dinputs" bits num_inputs)
      (create ~stages)
      i
  ;;
end

let hierarchical_general ~scope ~clock ~enable ~stages data =
  let bits = Signal.width (List.hd_exn data) in
  let module M =
    With_interface(struct
      let num_inputs = List.length data
      let bits = bits
    end)
  in
  let { M.O. difference; borrows; valid = _ } =
    M.hierarchical ~stages scope { clock; enable; data; valid = Signal.vdd }
  in
  { difference; borrows }
;;

let hierarchical ~scope ~clock ~enable ~stages a b =
  let { difference; borrows } =
    hierarchical_general ~scope ~clock ~enable ~stages [ a; b ]
  in
  let borrows =
    match borrows with
    | [ x ] -> x
    | _ -> assert false
  in
  { difference; borrows }
;;

module For_testing = struct
  let create_combinational (type a)
      (module Comb : Comb.S with type t = a)
      ~stages
      inputs =
    let open Implementation(Comb) in
    create ~stages ~pipe:(fun ~n:_ x -> x) inputs
  ;;
end
