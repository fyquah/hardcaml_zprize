open Core

type t = bytes

let of_scalars scalars =
  let num_scalars = List.length scalars in
  let buffer = Bytes.init (num_scalars * 48) ~f:(Fn.const '\000') in
  List.iteri scalars ~f:(fun i scalar ->
      let src = Bytes.unsafe_of_string_promise_no_mutation (Z.to_bits scalar) in
      Bytes.blito ~src ~dst:buffer ~dst_pos:(i * 48) ());
  buffer
;;
