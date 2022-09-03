open Base
open Hardcaml
open Reg_with_enable

module Single_op_output = struct
  type 'a t =
    { result : 'a
    ; carry : 'a
    }
  [@@deriving sexp_of, hardcaml, fields]
end

module Term_and_op = struct
  type 'a t =
    { op : [ `Add | `Sub ]
    ; term : 'a
    }
  [@@deriving fields]

  let map ~f { op; term } = { op; term = f term }
  let transpose { op; term } = List.map term ~f:(fun term -> { op; term })
end

(* [lhs `op` rhs[0] `op` rhs[1] `op` rhs[2] ... `op` rhs[-1]
 *
 * where `op` can be (+) or (-)
 *)
module Input = struct
  type 'a t =
    { lhs : 'a
    ; rhs_list : 'a Term_and_op.t list
    }

  let map ~f { lhs; rhs_list } =
    { lhs = f lhs; rhs_list = List.map ~f:(Term_and_op.map ~f) rhs_list }
  ;;

  let transpose { lhs; rhs_list } =
    List.map2_exn
      lhs
      (List.transpose_exn (List.map ~f:Term_and_op.transpose rhs_list))
      ~f:(fun lhs rhs_list -> { lhs; rhs_list })
  ;;

  let validate_same_width (type a) (module Comb : Comb.S with type t = a) (i : _ t) =
    let open Comb in
    let w = width i.lhs in
    List.iter i.rhs_list ~f:(fun x -> assert (width x.term = w));
    w
  ;;
end

module Output = struct
  type 'a t = 'a Single_op_output.t list

  let map ~f stage_output : _ t =
    List.map stage_output ~f:(fun ({ result; carry } : _ Single_op_output.t) ->
        { Single_op_output.carry = f carry; result = f result })
  ;;
end

module Make_stage (Comb : Comb.S) = struct
  open Comb

  module Term_and_op_and_carry = struct
    type t =
      { op : [ `Add | `Sub ]
      ; term : Comb.t
      ; carry_in : Comb.t
      }
  end

  let rec create_stage_impl
      prev
      (term_and_op_and_carry_list : Term_and_op_and_carry.t list)
    =
    match term_and_op_and_carry_list with
    | [] -> []
    | { Term_and_op_and_carry.term; op; carry_in } :: tl ->
      let this_sum =
        let w = width prev in
        let op =
          match op with
          | `Add -> ( +: )
          | `Sub -> ( -: )
        in
        assert (width term = w);
        op (op (uresize prev (w + 1)) (uresize term (w + 1))) (uresize carry_in (w + 1))
      in
      let hd = { Single_op_output.result = lsbs this_sum; carry = msb this_sum } in
      hd :: create_stage_impl (lsbs this_sum) tl
  ;;

  let create (prev_stage_output : _ Output.t) ({ lhs; rhs_list } : _ Input.t) : _ Output.t
    =
    assert (not (List.is_empty rhs_list));
    let part_width = width lhs in
    List.iter rhs_list ~f:(fun p -> assert (width p.term = part_width));
    let partial_output =
      List.map2_exn
        rhs_list
        prev_stage_output
        ~f:(fun { Term_and_op.term; op } { result = _; carry = carry_in } ->
          { Term_and_op_and_carry.term; op; carry_in })
      |> create_stage_impl lhs
    in
    List.map2_exn prev_stage_output partial_output ~f:(fun prev this ->
        { Single_op_output.result = concat_msb_e [ this.result; prev.result ]
        ; carry = this.carry
        })
  ;;
end

module Make_implementation (Comb : Comb.S) = struct
  open Comb
  module Stage = Make_stage (Comb)

  let create ~stages ~pipe (input : _ Input.t) =
    let bits = Input.validate_same_width (module Comb) input in
    let num_rhs_terms = List.length input.rhs_list in
    assert (stages <= bits);
    let part_width = Int.round_up ~to_multiple_of:stages bits / stages in
    assert (num_rhs_terms >= 1);
    (* [ a; b; c ] -> [ [ a1; b1; c1 ]; [ a2; b2; c2; ] [ a3; b3; c3 ] ]
     *
     * where a = concat_lsb [ a1; a2; a3 ]
     *)
    input
    |> Input.map ~f:(split_lsb ~exact:false ~part_width)
    |> Input.transpose
    |> List.foldi
         ~init:
           (List.init num_rhs_terms ~f:(fun _ ->
                { Single_op_output.result = empty; carry = gnd }))
         ~f:(fun n (acc : _ Output.t) (stage_input : _ Input.t) ->
           stage_input
           |> Input.map ~f:(pipe ~n)
           |> Stage.create acc
           |> Output.map ~f:(pipe ~n:1))
  ;;
end

module With_interface (M : sig
  val bits : int
  val rhs_list_length : int
end) =
struct
  module Implementation = Make_implementation (Signal)
  include M

  module Single_op_output =
    Interface.Update
      (Single_op_output)
      (struct
        let t = { Single_op_output.result = "result", bits; carry = "carry", 1 }
      end)

  module I = struct
    type 'a t =
      { clock : 'a
      ; enable : 'a
      ; lhs : 'a [@bits bits]
      ; rhs_list : 'a list [@bits bits] [@length rhs_list_length]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t = { results : 'a Single_op_output.t list [@length rhs_list_length] }
    [@@deriving sexp_of, hardcaml, fields]
  end

  let create
      ~stages
      ~ops
      ~rhs_constant_overrides
      (_ : Scope.t)
      { I.clock; enable; lhs; rhs_list }
    =
    let spec = Reg_spec.create ~clock () in
    let results =
      Implementation.create
        ~stages
        ~pipe:(fun ~n x ->
          assert (n >= 0);
          if Signal.is_const x || n = 0 then x else pipeline ~n spec ~enable x)
        { lhs
        ; rhs_list =
            List.map3_exn
              ops
              rhs_list
              rhs_constant_overrides
              ~f:(fun op term rhs_constant_override ->
                let term =
                  match rhs_constant_override with
                  | None -> term
                  | Some x -> Signal.of_constant (Bits.to_constant x)
                in
                { Term_and_op.op; term })
        }
    in
    { O.results }
  ;;

  let hierarchical ~instance ~name ~stages ~ops ~rhs_constant_overrides scope i =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ?instance ~scope ~name (create ~stages ~ops ~rhs_constant_overrides) i
  ;;
end

let hierarchical ?name ?instance ~stages ~scope ~enable ~clock (input : _ Input.t) =
  let bits = Input.validate_same_width (module Signal) input in
  let name =
    match name with
    | Some name -> name
    | None ->
      if List.for_all input.rhs_list ~f:(fun x -> Poly.equal x.op `Add)
      then Printf.sprintf "add_pipe_%d" bits
      else if List.for_all input.rhs_list ~f:(fun x -> Poly.equal x.op `Sub)
      then Printf.sprintf "sub_pipe_%d" bits
      else Printf.sprintf "add_sub_pipe_%d" bits
  in
  let module M =
    With_interface (struct
      let bits = bits
      let rhs_list_length = List.length input.rhs_list
    end)
  in
  let ops = List.map ~f:Term_and_op.op input.rhs_list in
  let rhs_list = List.map ~f:Term_and_op.term input.rhs_list in
  let rhs_constant_overrides =
    List.map rhs_list ~f:(function
        | Const { signal_id = _; constant } -> Some constant
        | _ -> None)
  in
  M.hierarchical
    ~instance
    ~rhs_constant_overrides
    ~name
    ~stages
    ~ops
    scope
    { clock; enable; lhs = input.lhs; rhs_list }
  |> M.O.results
;;

let create (type a) (module Comb : Comb.S with type t = a) ~stages ~pipe input =
  let module Impl = Make_implementation (Comb) in
  Impl.create ~stages ~pipe input
;;

let add ?name ?instance ~stages ~scope ~enable ~clock xs =
  hierarchical
    ?name
    ?instance
    ~stages
    ~scope
    ~enable
    ~clock
    { lhs = List.hd_exn xs
    ; rhs_list =
        List.map (List.tl_exn xs) ~f:(fun x -> { Term_and_op.op = `Add; term = x })
    }
  |> List.last_exn
  |> Single_op_output.result
;;

let sub ?name ?instance ~stages ~scope ~enable ~clock lhs rhs_list =
  hierarchical
    ?name
    ?instance
    ~stages
    ~scope
    ~enable
    ~clock
    { lhs
    ; rhs_list = List.map rhs_list ~f:(fun x -> { Term_and_op.op = `Sub; term = x })
    }
  |> List.last_exn
  |> Single_op_output.result
;;
