open Core
open Field_ops_lib
open Hardcaml

module Make (X : sig
  val bits : int
  val num_items : int
end) =
struct
  open X

  module I = struct
    type 'a t =
      { clock : 'a
      ; items : 'a array [@bits bits] [@length num_items]
      ; valid_in : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { sum : 'a [@bits bits]
      ; valid_out : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module Sim = Cyclesim.With_interface (I) (O)

  let create ~stages scope (i : _ I.t) =
    { O.sum =
        Adder_subtractor_pipe.add
          ~stages
          ~scope
          ~enable:Signal.vdd
          ~clock:i.clock
          (Array.to_list i.items)
    ; valid_out = Signal.pipeline ~n:stages (Reg_spec.create ~clock:i.clock ()) i.valid_in
    }
  ;;
end

let test ~bits ~num_items ~stages test_cases =
  let open Make (struct
    let num_items = num_items
    let bits = bits
  end) in
  let sim =
    let scope = Scope.create ~flatten_design:true () in
    Sim.create (create ~stages scope)
  in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let results = ref [] in
  let cycle () =
    Cyclesim.cycle sim;
    if Bits.is_vdd !(outputs.valid_out) then results := !(outputs.sum) :: !results
  in
  Array.iter test_cases ~f:(fun items ->
      inputs.valid_in := Bits.vdd;
      Array.iter2_exn inputs.items items ~f:(fun dst src -> dst := src);
      cycle ());
  inputs.valid_in := Bits.gnd;
  for _ = 1 to stages do
    cycle ()
  done;
  let results = Array.of_list (List.rev !results) in
  Array.iter2_exn test_cases results ~f:(fun test_case obtained ->
      let expected = Array.fold test_case ~init:(Bits.zero bits) ~f:Bits.( +: ) in
      if not (Bits.equal obtained expected)
      then raise_s [%message "Test case failed!" (bits : int)])
;;

let rand bits =
  Utils.random_z ~lo_incl:Z.zero ~hi_incl:Z.((one lsl bits) - one)
  |> Bits.of_z ~width:bits
;;

let test_random ~w ~k ~wx2 ~num_items ~stages ~num_test_cases =
  let bits = (2 * w) - (2 * k) in
  let k2 = 2 * k in
  let k3 = 3 * k in
  let k4 = 4 * k in
  let test_cases =
    Array.init num_test_cases ~f:(fun _ ->
        let open Bits in
        [| rand (wx2 * 2) @: zero (k4 - k2)
         ; uresize (rand (wx2 * 2) @: zero (k3 - k2)) ((w * 2) - k2)
         ; uresize (rand (wx2 * 2) @: zero (k3 - k2)) ((w * 2) - k2)
         ; uresize (rand (wx2 * 2)) ((w * 2) - k2)
         ; uresize (rand (wx2 * 2)) ((w * 2) - k2)
         ; uresize (rand (wx2 * 2)) ((w * 2) - k2)
        |])
  in
  test ~bits ~num_items ~stages test_cases
;;

let%expect_test "" =
  test_random ~num_items:6 ~num_test_cases:1_000 ~w:378 ~k:124 ~stages:5 ~wx2:130
;;
