open Core
open Hardcaml
open Signal

module Partition = struct
  type t =
    { slr : int option
    ; window_size_bits : int list
    }
  [@@deriving sexp_of]

  let num_windows t = List.length t.window_size_bits
end

let named_register = Field_ops_lib.Named_register.named_register

module Make (M : sig
  val centre_slr : int option
  val partitions : Partition.t list
  val data_bits : int

  (** The underlying XPM's read latency *)
  val ram_read_latency : int

  (** The latency on the inputs to the ram (read enables, read_window, write
       * enable and address *)
  val ram_lookup_latency : int

  (** Additional pipelining on the read data for it to cross SLR gracefully. *)
  val ram_output_latency : int
end) =
struct
  open M

  let num_windows =
    List.map partitions ~f:(fun p -> List.length p.window_size_bits)
    |> List.fold ~init:0 ~f:( + )
  ;;

  let address_bits =
    List.concat_map partitions ~f:(fun p -> p.window_size_bits)
    |> List.max_elt ~compare:Int.compare
    |> Option.value_exn
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

  let create ~build_mode ~b_write_data scope { I.clock; port_a; port_b } =
    (* TODO(fyquah): Do we need clear? *)
    let spec = Reg_spec.create ~clock () in
    let aqs, bqs =
      List.map2_exn window_offsets partitions ~f:(fun window_offset partition ->
        let module M =
          Window_ram_partition.Make (struct
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
          | Some slr -> Some (Printf.sprintf "window_ram_partition_SLR%d" slr)
        in
        (* TODO(fyquah): I thinkw e can get a way without clears?? *)
        let o =
          M.hierarchical
            ?instance
            ~build_mode
            ~b_write_data
            scope
            { clock
            ; port_a =
                { read_enables =
                    sublist port_a.read_enables
                    |> List.map ~f:(pipeline spec ~n:(ram_lookup_latency - 3))
                    |> concat_lsb
                    |> named_register ~scope ~clock ~slr:centre_slr
                    |> named_register ~scope ~clock ~slr:partition.slr
                    |> bits_lsb
                ; write_enables =
                    sublist port_a.write_enables
                    |> List.map ~f:(pipeline spec ~n:(ram_lookup_latency - 3))
                    |> concat_lsb
                    |> named_register ~scope ~clock ~slr:centre_slr
                    |> named_register ~scope ~clock ~slr:partition.slr
                    |> bits_lsb
                ; address =
                    pipeline spec ~n:(ram_lookup_latency - 3) port_a.address
                    |> named_register ~scope ~clock ~slr:centre_slr
                    |> named_register ~scope ~clock ~slr:partition.slr
                ; data =
                    pipeline spec ~n:(ram_lookup_latency - 3) port_a.data
                    |> named_register ~scope ~clock ~slr:centre_slr
                    |> named_register ~scope ~clock ~slr:partition.slr
                ; read_window =
                    (if Partition.num_windows partition = 1
                    then gnd
                    else
                      pipeline
                        spec
                        ~n:(ram_lookup_latency - 3)
                        (port_a.read_window -:. window_offset)
                      |> Fn.flip uresize (Int.ceil_log2 (Partition.num_windows partition))
                      |> named_register ~scope ~clock ~slr:centre_slr
                      |> named_register ~scope ~clock ~slr:partition.slr)
                }
            ; port_b =
                { read_enables =
                    sublist port_b.read_enables
                    |> List.map ~f:(pipeline spec ~n:(ram_lookup_latency - 3))
                    |> concat_lsb
                    |> named_register ~scope ~clock ~slr:centre_slr
                    |> named_register ~scope ~clock ~slr:partition.slr
                    |> bits_lsb
                ; write_enables =
                    sublist port_b.write_enables
                    |> List.map ~f:(pipeline spec ~n:(ram_lookup_latency - 3))
                    |> concat_lsb
                    |> named_register ~scope ~clock ~slr:centre_slr
                    |> named_register ~scope ~clock ~slr:partition.slr
                    |> bits_lsb
                ; address =
                    pipeline spec ~n:(ram_lookup_latency - 3) port_b.address
                    |> named_register ~scope ~clock ~slr:centre_slr
                    |> named_register ~scope ~clock ~slr:partition.slr
                ; data =
                    pipeline spec ~n:(ram_lookup_latency - 3) port_b.data
                    |> named_register ~scope ~clock ~slr:centre_slr
                    |> named_register ~scope ~clock ~slr:partition.slr
                ; read_window =
                    (if Partition.num_windows partition = 1
                    then gnd
                    else
                      port_b.read_window -:. window_offset
                      |> Fn.flip uresize (Int.ceil_log2 (Partition.num_windows partition))
                      |> pipeline spec ~n:(ram_lookup_latency - 3)
                      |> named_register ~scope ~clock ~slr:centre_slr
                      |> named_register ~scope ~clock ~slr:partition.slr)
                }
            }
        in
        ( pipeline spec ~n:(ram_output_latency - 3) o.port_a_q
          |> named_register ~scope ~clock ~slr:partition.slr
          |> named_register ~scope ~clock ~slr:centre_slr
        , pipeline spec ~n:(ram_output_latency - 3) o.port_b_q
          |> named_register ~scope ~clock ~slr:partition.slr
          |> named_register ~scope ~clock ~slr:centre_slr ))
      |> List.unzip
    in
    (* [read_partition_a] and [read_partition_b] doesn't require SLR pinning,
     * They should just be on the centre SLR. They are just used for muxing 
     * at the final stage.
     *)
    let read_partition_a =
      Option.map
        ~f:
          (pipeline
             spec
             ~n:(ram_lookup_latency + ram_read_latency + ram_output_latency - 1))
        (window_to_partition port_a.read_window)
    in
    let read_partition_b =
      Option.map
        ~f:
          (pipeline
             spec
             ~n:(ram_lookup_latency + ram_read_latency + ram_output_latency - 1))
        (window_to_partition port_b.read_window)
    in
    { O.port_a_q =
        (match read_partition_a with
         | None -> List.hd_exn aqs
         | Some read_partition_a -> mux read_partition_a aqs)
        |> pipeline spec ~n:1
    ; port_b_q =
        (match read_partition_b with
         | None -> List.hd_exn bqs
         | Some read_partition_b -> mux read_partition_b bqs)
        |> pipeline spec ~n:1
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
