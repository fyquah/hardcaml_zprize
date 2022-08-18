open! Core

let () =
  let t = Pippenger_test.Model.test_even ~verbose:true 10_000_000 in
  print_s [%message (t : Pippenger_test.Model.t)]
;;
