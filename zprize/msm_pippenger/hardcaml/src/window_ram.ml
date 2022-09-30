open Core
open Hardcaml
open Signal

module Partition = struct
  type t =
    { slr : int option
    ; window_size_bits : int list
    }
end

module Make (M : sig
  val partitions : Partition.t list
  val data_bits : int
  val ram_read_latency : int
  val ram_lookup_latency : int
end) =
struct
  open M

  let num_windows =
    List.map partitions ~f:(fun p -> List.length p.window_size_bits)
    |> List.fold ~init:0 ~f:( + )
  ;;

  let address_bits =
    Option.value_exn
      (List.max_elt
         ~compare:Int.max
         (List.concat_map partitions ~f:(fun p -> p.window_size_bits)))
  ;;

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
      ; clear : 'a
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

  let window_offsets =
    List.folding_map partitions ~init:0 ~f:(fun acc p ->
      acc + List.length p.window_size_bits, acc)
  ;;

  let log_num_partitions = Int.ceil_log2 (List.length partitions)

  let window_to_partition window =
    if log_num_partitions = 0
    then None
    else
      List.mapi window_offsets ~f:(fun i offset ->
        { With_valid.valid = window >=:. offset
        ; value = Signal.of_int ~width:log_num_partitions i
        })
      |> List.rev
      |> Signal.priority_select_with_default ~default:(zero log_num_partitions)
      |> Some
  ;;

  let create ~build_mode ~b_write_data scope { I.clock; clear; port_a; port_b } =
    (* TODO(fyquah): Do we need clear? *)
    let spec = Reg_spec.create ~clock ~clear () in
    let aqs, bqs =
      List.map2_exn window_offsets partitions ~f:(fun window_offset partition ->
        let module M =
          Window_ram_for_slr.Make (struct
            let address_bits = address_bits
            let window_size_bits = partition.window_size_bits
            let data_bits = data_bits
            let ram_read_latency = ram_read_latency
          end)
        in
        let sublist l =
          List.sub l ~pos:window_offset ~len:(List.length partition.window_size_bits)
        in
        let instance =
          match partition.slr with
          | None -> None
          | Some slr -> Some (Printf.sprintf "window_ram_for_slr_SLR%d" slr)
        in
        let o =
          M.hierarchical
            ?instance
            ~build_mode
            ~b_write_data
            scope
            { clock
            ; clear
            ; port_a =
                { read_enables =
                    List.map
                      (sublist port_a.read_enables)
                      ~f:(pipeline spec ~n:ram_lookup_latency)
                ; write_enables =
                    List.map
                      (sublist port_a.write_enables)
                      ~f:(pipeline spec ~n:ram_lookup_latency)
                ; address = pipeline spec ~n:ram_lookup_latency port_a.address
                ; data = pipeline spec ~n:ram_lookup_latency port_a.data
                ; read_window =
                    pipeline
                      spec
                      ~n:(ram_lookup_latency + ram_read_latency)
                      (port_a.read_window -:. window_offset)
                }
            ; port_b =
                { read_enables =
                    List.map
                      ~f:(pipeline spec ~n:ram_lookup_latency)
                      (sublist port_b.read_enables)
                ; write_enables =
                    List.map
                      ~f:(pipeline spec ~n:ram_lookup_latency)
                      (sublist port_b.write_enables)
                ; address = pipeline spec ~n:ram_lookup_latency port_b.address
                ; data = pipeline spec ~n:ram_lookup_latency port_b.data
                ; read_window =
                    pipeline
                      spec
                      ~n:(ram_lookup_latency + ram_read_latency)
                      (port_b.read_window -:. window_offset)
                }
            }
        in
        o.port_a_q, o.port_b_q)
      |> List.unzip
    in
    let read_partition_a =
      Option.map
        ~f:(pipeline spec ~n:(ram_lookup_latency + ram_read_latency))
        (window_to_partition port_a.read_window)
    in
    let read_partition_b =
      Option.map
        ~f:(pipeline spec ~n:(ram_lookup_latency + ram_read_latency))
        (window_to_partition port_b.read_window)
    in
    { O.port_a_q =
        (match read_partition_a with
         | None -> List.hd_exn aqs
         | Some read_partition_a -> mux read_partition_a aqs)
    ; port_b_q =
        (match read_partition_b with
         | None -> List.hd_exn aqs
         | Some read_partition_b -> mux read_partition_b bqs)
    }
  ;;

  let hierarchical ?instance ~b_write_data ~build_mode scope i =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical
      ~name:"window_ram"
      ?instance
      ~scope
      (create ~build_mode ~b_write_data)
      i
  ;;
end
