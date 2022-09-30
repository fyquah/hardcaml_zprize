open Core
open Hardcaml
open Signal
open Hardcaml_xilinx

module Make (M : sig
  val window_size_bits : int list
  val data_bits : int
  val ram_read_latency : int
end) =
struct
  open M

  let num_windows = List.length window_size_bits

  module I = struct
    module Ram_port = struct
      type 'a t =
        { write_enables : 'a list [@length num_windows]
        ; read_enables : 'a list [@length num_windows]
        ; data : 'a [@bits data_bits]
        ; address : 'a
             [@bits Option.value_exn (List.max_elt ~compare:Int.max window_size_bits)]
        }
      [@@deriving sexp_of, hardcaml]
    end

    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; port_a : 'a Ram_port.t [@rtlprefix "a$"]
      ; port_b : 'a Ram_port.t [@rtlprefix "b$"]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { port_a_q : 'a list [@bits data_bits] [@length num_windows]
      ; port_b_q : 'a list [@bits data_bits] [@length num_windows]
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create ~build_mode ~b_write_data scope { I.clock; clear; port_a; port_b } =
    let ( -- ) = Scope.naming scope in
    let port_a_q, port_b_q =
      List.mapi window_size_bits ~f:(fun i window_size_bits ->
        Dual_port_ram.create
          ~read_latency:ram_read_latency
          ~arch:Ultraram
          ~build_mode
          ~clock
          ~clear
          ~size:(1 lsl window_size_bits)
          ~port_a:
            (let port =
               { Ram_port.write_enable = List.nth_exn port_a.write_enables i
               ; read_enable = List.nth_exn port_a.read_enables i
               ; data = port_a.data
               ; address = sel_bottom port_a.address window_size_bits
               }
             in
             Ram_port.(
               iter2 port port_names ~f:(fun s n ->
                 ignore (s -- ("window" ^ Int.to_string i ^ "$ram_a$" ^ n) : Signal.t)));
             port)
          ~port_b:
            (let port =
               { Ram_port.write_enable = List.nth_exn port_b.write_enables i
               ; read_enable = List.nth_exn port_b.read_enables i
               ; data = b_write_data
               ; address = sel_bottom port_b.address window_size_bits
               }
             in
             Ram_port.(
               iter2 port port_names ~f:(fun s n ->
                 ignore (s -- ("window" ^ Int.to_string i ^ "$ram_b$" ^ n) : Signal.t)));
             port)
          ())
      |> List.unzip
    in
    List.iteri port_a_q ~f:(fun i port ->
      ignore (port -- ("window" ^ Int.to_string i ^ "$ram_a$q") : Signal.t));
    List.iteri port_b_q ~f:(fun i port ->
      ignore (port -- ("window" ^ Int.to_string i ^ "$ram_b$q") : Signal.t));
    { O.port_a_q; port_b_q }
  ;;

  let hierarchical ?instance ~b_write_data ~build_mode scope i =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical
      ~attributes:[ Rtl_attribute.Vivado.keep_hierarchy true ]
      ~name:"window_ram"
      ?instance
      ~scope
      (create ~build_mode ~b_write_data)
      i
  ;;
end
