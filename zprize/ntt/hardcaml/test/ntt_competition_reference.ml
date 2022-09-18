open Base
open Hardcaml
open! Bits

let debugging = false

module Make (Gf : Hardcaml_ntt.Gf.S) = struct
  let rec loop3 ~input ~i ~j ~k ~m ~w ~w_m =
    if j >= m
    then ()
    else (
      let t1 = input.(k + j) in
      let t2 = input.(k + j + m) in
      let t = Gf.(t2 * w) in
      input.(k + j) <- Gf.(t1 + t);
      input.(k + j + m) <- Gf.(t1 - t);
      if debugging
      then
        Stdio.printf
          "%i %i %s %s %s %s\n"
          (k + j)
          (k + j + m)
          (Gf.to_z t1 |> Z.to_string)
          (Gf.to_z t2 |> Z.to_string)
          (Gf.to_z input.(k + j) |> Z.to_string)
          (Gf.to_z input.(k + j + m) |> Z.to_string);
      loop3 ~input ~i ~j:(j + 1) ~k ~m ~w:Gf.(w * w_m) ~w_m)
  ;;

  let rec loop2 ~input ~i ~k ~m ~n ~w_m =
    if k >= n
    then ()
    else (
      loop3 ~input ~i ~j:0 ~k ~m ~w:Gf.one ~w_m;
      loop2 ~input ~i ~k:(k + (2 * m)) ~m ~n ~w_m)
  ;;

  let omega i = Hardcaml_ntt.Roots.inverse.(i) |> Hardcaml_ntt.Gf.Z.to_z |> Gf.of_z

  let rec loop1 ~input ~logn ~i ~m =
    if i > logn
    then ()
    else (
      loop2 ~input ~i ~k:0 ~m ~n:(1 lsl logn) ~w_m:(omega i);
      loop1 ~input ~logn ~i:(i + 1) ~m:(m * 2))
  ;;

  let ntt input =
    Hardcaml_ntt.Util.bit_reversed_addressing input;
    let logn = Int.ceil_log2 (Array.length input) in
    loop1 ~input ~logn ~i:1 ~m:1
  ;;
end
