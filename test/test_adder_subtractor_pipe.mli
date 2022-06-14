open Core

val test
  :  op:[ `Add | `Sub ]
  -> bits:int
  -> stages:int
  -> num_inputs:int
  -> unit Or_error.t
