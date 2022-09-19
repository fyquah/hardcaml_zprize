open Base
open Hardcaml
open! Bits
module Gf_z = Gf.Z

module Make (Gf : Gf.S) = struct
  let forward_roots = Array.map Roots.forward ~f:(fun z -> Gf_z.to_z z |> Gf.of_z)
  let inverse_roots = Array.map Roots.inverse ~f:(fun z -> Gf_z.to_z z |> Gf.of_z)

  let dit omega a =
    Util.bit_reversed_addressing a;
    let n = Array.length a in
    let logn = Int.ceil_log2 n in
    for s = 1 to logn do
      let m = 1 lsl s in
      let wm = omega.(s) in
      let k = ref 0 in
      while !k < n do
        let w = ref Gf.one in
        for j = 0 to (m / 2) - 1 do
          let u = a.(!k + j) in
          let v = Gf.( * ) !w a.(!k + j + (m / 2)) in
          a.(!k + j) <- Gf.(u + v);
          a.(!k + j + (m / 2)) <- Gf.(u - v);
          w := Gf.(!w * wm)
        done;
        k := !k + m
      done
    done
  ;;

  let dif omega a =
    let n = Array.length a in
    let logn = Int.ceil_log2 n in
    for s = logn downto 1 do
      let m = 1 lsl s in
      let wm = omega.(s) in
      let k = ref 0 in
      while !k < n do
        let w = ref Gf.one in
        for j = 0 to (m / 2) - 1 do
          let u = a.(!k + j) in
          let v = a.(!k + j + (m / 2)) in
          a.(!k + j) <- Gf.(u + v);
          a.(!k + j + (m / 2)) <- Gf.(!w * (u - v));
          w := Gf.(!w * wm)
        done;
        k := !k + m
      done
    done;
    Util.bit_reversed_addressing a
  ;;

  let forward_dit = dit forward_roots
  let inverse_dit = dit inverse_roots
  let forward_dif = dif forward_roots
  let inverse_dif = dif inverse_roots

  let matrix a log_rows log_cols =
    let rows = 1 lsl log_rows in
    let cols = 1 lsl log_cols in
    Array.init rows ~f:(fun row -> Array.init cols ~f:(fun col -> a.((row * cols) + col)))
  ;;

  let transpose a =
    let rows = Array.length a in
    let cols = Array.length a.(0) in
    Array.init cols ~f:(fun col -> Array.init rows ~f:(fun row -> a.(row).(col)))
  ;;

  let row a row = Array.copy a.(row)
  let col a col = Array.init (Array.length a) ~f:(fun row -> a.(row).(col))

  let apply_twiddles wm a =
    let rec row wm w a index =
      if index = Array.length a
      then ()
      else (
        a.(index) <- Gf.(w * a.(index));
        row wm Gf.(wm * w) a (index + 1))
    in
    let rec f wm w index =
      if index = Array.length a
      then ()
      else (
        row w Gf.one a.(index) 0;
        f wm Gf.(w * wm) (index + 1))
    in
    f wm Gf.one 0
  ;;

  let four_step a log_rows =
    let n = Array.length a in
    let logn = Int.ceil_log2 n in
    let log_cols = logn - log_rows in
    assert (log_rows > 0);
    assert (log_cols > 0);
    let matrix = matrix a log_cols log_rows |> transpose in
    Array.iter matrix ~f:inverse_dit;
    apply_twiddles inverse_roots.(logn) matrix;
    let matrix = transpose matrix in
    Array.iter matrix ~f:inverse_dit;
    let matrix = transpose matrix in
    Array.concat (Array.to_list matrix)
  ;;
end
