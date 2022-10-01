open Base
open Hardcaml
open Reg_with_enable

(* CR rahul for fyquah: this correct? *)
let latency ~stages = stages

module Term_and_op = struct
  type 'a t =
    | Add of 'a
    | Sub of 'a

  let map ~f = function
    | Add x -> Add (f x)
    | Sub x -> Sub (f x)
  ;;

  let transpose = function
    | Add xs -> List.map xs ~f:(fun term -> Add term)
    | Sub xs -> List.map xs ~f:(fun term -> Sub term)
  ;;

  let term = function
    | Add x -> x
    | Sub x -> x
  ;;

  let is_add = function
    | Add _ -> true
    | Sub _ -> false
  ;;

  let is_sub = function
    | Add _ -> false
    | Sub _ -> true
  ;;

  let op = function
    | Add _ -> `Add
    | Sub _ -> `Sub
  ;;
end

(* [lhs `op` rhs[0] `op` rhs[1] `op` rhs[2] ... `op` rhs[-1]
 *
 * where `op` can be (+) or (-)
 *)
module I = struct
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
    List.iter i.rhs_list ~f:(fun x -> assert (width (Term_and_op.term x) = w));
    w
  ;;
end

module O = struct
  type 'a t =
    { result : 'a
    ; carry : 'a
    }
  [@@deriving sexp_of, hardcaml, fields]
end

module Carry_type = struct
  type t =
    | Carry
    | Borrow
    | With_sign_bit
  [@@deriving sexp_of, equal]
end

module Make_stage (Comb : Comb.S) = struct
  open Comb

  let create_stage_impl prev terms =
    List.fold terms ~init:prev ~f:(fun acc term ->
      let op, term =
        match term with
        | Term_and_op.Add term -> ( +: ), term
        | Term_and_op.Sub term -> ( -: ), term
      in
      let w = width acc in
      op acc (uresize term w))
  ;;

  let create (prev_stage_output : _ O.t) ~carry_num_bits ~carry_type ~init items : _ O.t =
    assert (not (List.is_empty items));
    let w = width init in
    List.iter items ~f:(fun p ->
      let term =
        match p with
        | Term_and_op.Add p -> p
        | Term_and_op.Sub p -> p
      in
      assert (width term = w));
    let partial_sum_and_carry =
      let init = uresize init (w + carry_num_bits) in
      create_stage_impl
        (match (carry_type : Carry_type.t) with
         | Carry -> init +: uresize prev_stage_output.carry (w + carry_num_bits)
         | Borrow -> init -: uresize prev_stage_output.carry (w + carry_num_bits)
         | With_sign_bit -> init +: sresize prev_stage_output.carry (w + carry_num_bits))
        items
    in
    let carry = sel_top partial_sum_and_carry carry_num_bits in
    let carry =
      match carry_type with
      | Borrow -> negate carry
      | Carry | With_sign_bit -> carry
    in
    { O.result =
        concat_msb_e
          [ drop_top partial_sum_and_carry carry_num_bits; prev_stage_output.result ]
    ; carry
    }
  ;;
end

module Make_implementation (Comb : Comb.S) = struct
  open Comb
  module Stage = Make_stage (Comb)

  let create ~stages ~pipe ~carry_type ~carry_num_bits (input : _ I.t) =
    let bits = I.validate_same_width (module Comb) input in
    let num_rhs_terms = List.length input.rhs_list in
    let part_width = Int.round_up ~to_multiple_of:stages bits / stages in
    assert (stages <= bits);
    assert (num_rhs_terms >= 1);
    (* [ a; b; c ] -> [ [ a1; b1; c1 ]; [ a2; b2; c2; ] [ a3; b3; c3 ] ]
     *
     * where a = concat_lsb [ a1; a2; a3 ]
     *)
    input
    |> I.map ~f:(split_lsb ~exact:false ~part_width)
    |> I.transpose
    |> List.foldi
         ~init:{ O.result = empty; carry = zero carry_num_bits }
         ~f:(fun n (acc : _ O.t) (stage_input : _ I.t) ->
           let { I.lhs; rhs_list } = I.map ~f:(pipe ~n) stage_input in
           rhs_list
           |> Stage.create ~carry_type ~carry_num_bits acc ~init:lhs
           |> O.map ~f:(pipe ~n:1))
  ;;
end

module With_interface (M : sig
  val bits : int
  val rhs_list_length : int
  val carry_num_bits : int
end) =
struct
  module Implementation = Make_implementation (Signal)
  include M

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
    type 'a t = 'a O.t =
      { result : 'a [@bits bits]
      ; carry : 'a [@bits carry_num_bits]
      }
    [@@deriving sexp_of, hardcaml, fields]
  end

  let create
    ~stages
    ~ops
    ~rhs_constant_overrides
    ~carry_type
    (_ : Scope.t)
    { I.clock; enable; lhs; rhs_list }
    =
    let spec = Reg_spec.create ~clock () in
    let rhs_list =
      List.map2_exn rhs_list rhs_constant_overrides ~f:(fun default override ->
        Option.map override ~f:(fun x -> Signal.of_constant (Bits.to_constant x))
        |> Option.value ~default)
      |> List.map2_exn ops ~f:(fun op term ->
           match op with
           | `Add -> Term_and_op.Add term
           | `Sub -> Term_and_op.Sub term)
    in
    Implementation.create
      ~carry_num_bits
      ~carry_type
      ~stages
      ~pipe:(fun ~n x ->
        assert (n >= 0);
        if Signal.is_const x || n = 0 then x else pipeline ~n spec ~enable x)
      { lhs; rhs_list }
  ;;

  let hierarchical
    ~instance
    ~name
    ~stages
    ~ops
    ~rhs_constant_overrides
    ~carry_type
    scope
    i
    =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical
      ?instance
      ~scope
      ~name
      (create ~stages ~ops ~rhs_constant_overrides ~carry_type)
      i
  ;;
end

let mixed ?name ?instance ~stages ~scope ~enable ~clock ~init items =
  let input = { I.lhs = init; rhs_list = items } in
  let carry_type =
    if List.for_all input.rhs_list ~f:Term_and_op.is_add
    then Carry_type.Carry
    else if List.for_all input.rhs_list ~f:Term_and_op.is_sub
    then Carry_type.Borrow
    else Carry_type.With_sign_bit
  in
  let carry_num_bits =
    (* CR fyquah: This is perhaps too coarse? i think we can do better *)
    Int.ceil_log2 (1 + List.length input.rhs_list)
  in
  let carry_num_bits =
    if Carry_type.equal With_sign_bit carry_type
    then carry_num_bits + 1
    else carry_num_bits
  in
  let bits = I.validate_same_width (module Signal) input in
  let name =
    match name with
    | Some name -> name
    | None ->
      if List.for_all input.rhs_list ~f:Term_and_op.is_add
      then Printf.sprintf "add_pipe_%d" bits
      else if List.for_all input.rhs_list ~f:Term_and_op.is_sub
      then Printf.sprintf "sub_pipe_%d" bits
      else Printf.sprintf "add_sub_pipe_%d" bits
  in
  let module M =
    With_interface (struct
      let bits = bits
      let rhs_list_length = List.length input.rhs_list
      let carry_num_bits = carry_num_bits
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
    ~carry_type
    scope
    { clock; enable; lhs = input.lhs; rhs_list }
;;

let add ?name ?instance ~stages ~scope ~enable ~clock xs =
  mixed
    ?name
    ?instance
    ~stages
    ~scope
    ~enable
    ~clock
    ~init:(List.hd_exn xs)
    (List.map (List.tl_exn xs) ~f:(fun x -> Term_and_op.Add x))
;;

let sub ?name ?instance ~stages ~scope ~enable ~clock xs =
  mixed
    ?name
    ?instance
    ~stages
    ~scope
    ~enable
    ~clock
    ~init:(List.hd_exn xs)
    (List.map (List.tl_exn xs) ~f:(fun x -> Term_and_op.Sub x))
;;

let mixed_no_carry ?name ?instance ~stages ~scope ~enable ~clock ~init items =
  match stages with
  | 0 | 1 ->
    List.fold ~init items ~f:(fun acc item ->
      match item with
      | Term_and_op.Add x -> Signal.( +: ) acc x
      | Term_and_op.Sub x -> Signal.( -: ) acc x)
    |> pipeline ~n:stages ~enable (Reg_spec.create ~clock ())
  | _ -> mixed ?name ?instance ~stages ~scope ~enable ~clock ~init items |> O.result
;;

let add_no_carry ?name ?instance ~stages ~scope ~enable ~clock xs =
  mixed_no_carry
    ?name
    ?instance
    ~stages
    ~scope
    ~enable
    ~clock
    ~init:(List.hd_exn xs)
    (List.map (List.tl_exn xs) ~f:(fun x -> Term_and_op.Add x))
;;

let sub_no_carry ?name ?instance ~stages ~scope ~enable ~clock xs =
  mixed_no_carry
    ?name
    ?instance
    ~stages
    ~scope
    ~enable
    ~clock
    ~init:(List.hd_exn xs)
    (List.map (List.tl_exn xs) ~f:(fun x -> Term_and_op.Sub x))
;;
