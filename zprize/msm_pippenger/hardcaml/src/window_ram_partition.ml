open Core
open Hardcaml
open Hardcaml_xilinx
open Signal

module Slr = struct
  type t =
    | SLR0
    | SLR1
    | SLR2
end

module Make (M : sig
  val address_bits : int
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
        ; address : 'a [@bits address_bits]
        ; read_window : 'a [@bits Int.ceil_log2 num_windows]
        }
      [@@deriving sexp_of, hardcaml]
    end

    type 'a t =
      { clock : 'a
      ; port_a : 'a Ram_port.t [@rtlprefix "a$"]
      ; port_b : 'a Ram_port.t [@rtlprefix "b$"]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { port_a_q : 'a [@bits data_bits]
      ; port_b_q : 'a [@bits data_bits]
      }
    [@@deriving sexp_of, hardcaml]
  end

  (** [b_write_data] is a constant, so it doesn't need any registers around
   * it.
   *)
  let create ~build_mode ~b_write_data scope { I.clock; port_a; port_b } =
    let ( -- ) = Scope.naming scope in
    let spec = Reg_spec.create ~clock () in
    let port_a_q, port_b_q =
      List.mapi window_size_bits ~f:(fun i window_size_bits ->
        if window_size_bits > address_bits
        then raise_s [%message (window_size_bits : int) (address_bits : int)];
        Dual_port_ram.create
          ~read_latency:ram_read_latency
          ~arch:Ultraram
          ~build_mode
          ~clock
          ~clear:gnd
          ~size:(1 lsl window_size_bits)
          ~port_a:
            (let port =
               { Ram_port.write_enable = reg spec (List.nth_exn port_a.write_enables i)
               ; read_enable = reg spec (List.nth_exn port_a.read_enables i)
               ; data = reg spec port_a.data
               ; address = reg spec (sel_bottom port_a.address window_size_bits)
               }
             in
             Ram_port.(
               iter2 port port_names ~f:(fun s n ->
                 ignore (s -- ("window" ^ Int.to_string i ^ "$ram_a$" ^ n) : Signal.t)));
             port)
          ~port_b:
            (let port =
               { Ram_port.write_enable = reg spec (List.nth_exn port_b.write_enables i)
               ; read_enable = reg spec (List.nth_exn port_b.read_enables i)
               ; data = b_write_data
               ; address = reg spec (sel_bottom port_b.address window_size_bits)
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
    { O.port_a_q =
        (match port_a_q with
         | [ hd ] -> hd
         | _ ->
           mux
             (Signal.pipeline spec ~n:(ram_read_latency + 1) port_a.read_window)
             port_a_q)
    ; port_b_q =
        (match port_b_q with
         | [ hd ] -> hd
         | _ ->
           mux
             (Signal.pipeline spec ~n:(ram_read_latency + 1) port_b.read_window)
             port_b_q)
    }
  ;;

  let hierarchical ?instance ~b_write_data ~build_mode scope i =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical
      ~name:"window_ram_partition"
      ~attributes:[ Rtl_attribute.Vivado.keep_hierarchy true ]
      ?instance
      ~scope
      (create ~build_mode ~b_write_data)
      i
  ;;
end
