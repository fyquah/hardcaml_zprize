open Base

let reverse logn x =
  let rec reverse logn x acc =
    if logn = 0 then acc else reverse (logn - 1) (x lsr 1) ((acc lsl 1) lor (x land 1))
  in
  reverse logn x 0
;;

let bit_reversed_addressing input =
  let logn = Int.ceil_log2 (Array.length input) in
  let max = (1 lsl logn) - 1 in
  let rec loop k =
    let rk = reverse logn k in
    if k < rk
    then (
      let tmp = input.(rk) in
      input.(rk) <- input.(k);
      input.(k) <- tmp);
    if k = max then () else loop (k + 1)
  in
  loop 0
;;
