(* *)
open! Base
open! Hardcaml
open! Bits
module Gf = Gf.Make (Bits)

let bit_reversed_addressing input =
  let logn = Int.ceil_log2 (Array.length input) in
  let max = ones logn in
  let rec loop k =
    let rk = reverse k in
    if Bits.equal (k <: rk) vdd
    then (
      let tmp = input.(to_int rk) in
      input.(to_int rk) <- input.(to_int k);
      input.(to_int k) <- tmp);
    if Bits.equal k max then () else loop (k +:. 1)
  in
  loop (zero logn)
;;

let rec loop3 ~input ~i ~j ~k ~m ~w ~w_m =
  if j >= m
  then ()
  else (
    let t = input.(k + j + m) in
    let t = Gf.(t *: w) in
    let tmp = input.(k + j) in
    let tmp = Gf.(tmp -: t) in
    input.(k + j + m) <- tmp;
    input.(k + j) <- Gf.(input.(k + j) +: t);
    loop3 ~input ~i ~j:(j + 1) ~k ~m ~w:Gf.(w *: w_m) ~w_m)
;;

let rec loop2 ~input ~i ~k ~m ~n ~w_m =
  if k >= n
  then ()
  else (
    loop3 ~input ~i ~j:0 ~k ~m ~w:Gf.one ~w_m;
    loop2 ~input ~i ~k:(k + (2 * m)) ~m ~n ~w_m)
;;

let rec loop1 ~input ~logn ~i ~m =
  if i > logn
  then ()
  else (
    loop2 ~input ~i ~k:0 ~m ~n:(1 lsl logn) ~w_m:Gf.omega.(i);
    loop1 ~input ~logn ~i:(i + 1) ~m:(m * 2))
;;

let ntt input =
  bit_reversed_addressing input;
  let logn = Int.ceil_log2 (Array.length input) in
  loop1 ~input ~logn ~i:1 ~m:1
;;
