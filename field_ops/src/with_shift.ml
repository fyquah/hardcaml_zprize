open Core
open Hardcaml
open Signal

type t =
  { x : Signal.t
  ; shift : int
  }

let width t = Signal.width t.x + t.shift
let no_shift x = { x; shift = 0 }
let create ~shift x = { x; shift }
let sll t ~by = { x = t.x; shift = t.shift + by }
let map ~f t = { x = f t.x; shift = t.shift }

let uresize t new_width =
  { x = Signal.uresize t.x (new_width - t.shift); shift = t.shift }
;;

let validate_all_items_same_width items =
  let w = width (List.hd_exn items) in
  List.iter items ~f:(fun x -> assert (width x = w));
  w
;;

let on_overlapping_bits (items : t list) do_thing =
  let item_width = validate_all_items_same_width items in
  let smallest_shift =
    Option.value_exn
      (List.min_elt ~compare:Int.compare (List.map items ~f:(fun i -> i.shift)))
  in
  let x =
    List.map items ~f:(fun item ->
        let signal =
          match item.shift - smallest_shift with
          | 0 -> item.x
          | shift -> item.x @: zero shift
        in
        Signal.uresize signal (item_width - smallest_shift))
    |> do_thing
  in
  { x; shift = smallest_shift }
;;

let pipe_add ~scope ~enable ~clock ~stages (items : t list) =
  on_overlapping_bits items (fun x ->
      x |> Adder_subtractor_pipe.add_no_carry ~scope ~enable ~clock ~stages)
;;

let sum (items : t list) =
  on_overlapping_bits items (fun x -> List.reduce_exn x ~f:( +: ))
;;

let peek_terms a =
  List.map a ~f:(function
      | `Add x -> x
      | `Sub x -> x)
;;

let mixed ~init arg_items =
  on_overlapping_bits (init :: peek_terms arg_items) (fun x ->
      let init, tl =
        match x with
        | hd :: tl -> hd, tl
        | _ -> assert false
      in
      let tl =
        List.map2_exn arg_items tl ~f:(fun arg_item x ->
            match arg_item with
            | `Add _ -> `Add x
            | `Sub _ -> `Sub x)
      in
      List.fold tl ~init ~f:(fun unchanged term ->
          match term with
          | `Add x -> unchanged +: x
          | `Sub x -> unchanged -: x))
;;

let to_signal t = if t.shift = 0 then t.x else t.x @: zero t.shift
