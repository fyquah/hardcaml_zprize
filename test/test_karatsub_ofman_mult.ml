open! Base
open! Hardcaml
open! Snarks_r_fun

module Single_depth_mult = Karatsuba_ofman_mult.With_interface(struct
    let num_bits = 32
    let depth = 1
  end)

let create_sim () =
  let scope = Scope.create ~flatten_design:true () in
  let module Sim = Cyclesim.With_interface(Single_depth_mult.I)(Single_depth_mult.O) in
  Sim.create (Single_depth_mult.create scope)
;;

let%expect_test "" =
  let _sim = create_sim () in
  Stdio.printf "hello there\n";
  ()
;;
