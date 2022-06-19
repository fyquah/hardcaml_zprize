open Base
open Hardcaml
open Signal

let reg spec ~enable x = reg spec ~enable x

let pipeline spec ~enable ~n x =
  if n < 0
  then raise_s [%message "pipeline cannot take a negative integer as argument!" (n : int)];
  pipeline spec ~enable ~n x
;;
