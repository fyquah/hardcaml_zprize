open Base
open Hardcaml
open Signal

module Make (X : Config.S) = struct
  open X

  let strb_bits = data_bits / 8
  let () = assert (data_bits % 8 = 0)

  module Master_to_slave = struct
    type 'a t =
      { awaddr : 'a [@bits addr_bits]
      ; awvalid : 'a
      ; wdata : 'a [@bits data_bits]
      ; wstrb : 'a [@bits strb_bits]
      ; wvalid : 'a
      ; bready : 'a
      ; araddr : 'a [@bits addr_bits]
      ; arvalid : 'a
      ; rready : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module Slave_to_master = struct
    type 'a t =
      { awready : 'a
      ; wready : 'a
      ; bresp : 'a [@bits 2]
      ; bvalid : 'a
      ; arready : 'a
      ; rdata : 'a [@bits data_bits]
      ; rresp : 'a [@bits 2]
      ; rvalid : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module Register_bank (Rd : Hardcaml.Interface.S) (Wr : Hardcaml.Interface.S) = struct
    module I = struct
      type 'a t =
        { clock : 'a
        ; clear : 'a
        ; master_to_slave : 'a Master_to_slave.t
        ; read_values : 'a Rd.t
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O = struct
      type 'a t =
        { slave_to_master : 'a Slave_to_master.t
        ; write_values : 'a Wr.t
        }
      [@@deriving sexp_of, hardcaml]
    end

    module State = struct
      type t =
        | S_idle
        | S_read_address
        | S_read_data
        | S_write_address
        | S_wait_write_data
        | S_send_write_response
      [@@deriving sexp_of, enumerate, compare]
    end

    let num_read_entries = Rd.fold Rd.t ~f:(fun a _ -> a + 1) ~init:0
    let log_num_read_entries = Int.ceil_log2 num_read_entries

    let () =
      Rd.iter Rd.t ~f:(fun (name, w) ->
          if w > 32
          then
            raise_s
              [%message
                "Register bank port must be less than or equal 32 bits wide"
                  (name : string)])
    ;;

    let create (_ : Scope.t) { I.clock; clear; master_to_slave; read_values } =
      let spec = Reg_spec.create ~clock ~clear () in
      let sm = Always.State_machine.create (module State) spec in
      let slave_to_master = Slave_to_master.Of_always.reg spec in
      let write_address = Always.Variable.reg spec ~width:addr_bits in
      let write_data = Always.Variable.reg spec ~width:data_bits in
      let write_enable = Always.Variable.reg spec ~width:1 in
      Always.(
        compile
          [ slave_to_master.arready <--. 0
          ; write_enable <--. 0
          ; sm.switch
              [ ( S_idle
                , [ if_
                      master_to_slave.arvalid
                      [ slave_to_master.arready <--. 1; sm.set_next S_read_address ]
                    @@ elif
                         master_to_slave.awvalid
                         [ write_address <-- master_to_slave.awaddr
                         ; slave_to_master.awready <--. 1
                         ; sm.set_next S_write_address
                         ]
                    @@ []
                  ] )
              ; ( S_read_address
                , [ slave_to_master.rvalid <--. 1
                  ; slave_to_master.rdata
                    <-- mux
                          (sel_bottom
                             (drop_bottom master_to_slave.araddr 2)
                             log_num_read_entries)
                          (List.map ~f:(Fn.flip uresize 32) (Rd.to_list read_values))
                  ; slave_to_master.rresp <--. 0
                  ; sm.set_next S_read_data
                  ] )
              ; ( S_read_data
                , [ when_
                      master_to_slave.rready
                      [ slave_to_master.rvalid <--. 0; sm.set_next S_idle ]
                  ] )
              ; ( S_write_address
                , [ slave_to_master.awready <--. 0
                  ; slave_to_master.wready <--. 1
                  ; sm.set_next S_wait_write_data
                  ] )
              ; ( S_wait_write_data
                , [ when_
                      master_to_slave.wvalid
                      [ write_data <-- master_to_slave.wdata
                      ; slave_to_master.wready <--. 0
                      ; slave_to_master.bvalid <--. 1
                      ; slave_to_master.bresp <--. 0
                      ; write_enable <--. 1
                      ; sm.set_next S_send_write_response
                      ]
                  ] )
              ; ( S_send_write_response
                , [ when_
                      master_to_slave.bready
                      [ slave_to_master.bvalid <--. 0; sm.set_next S_idle ]
                  ] )
              ]
          ]);
      let write_values =
        let write_addresses =
          let r = Wr.map ~f:(fun _ -> ref 0) Wr.t in
          let i = ref 0 in
          Wr.iter r ~f:(fun r ->
              r := !i;
              i := !i + 4);
          Wr.map ~f:( ! ) r
        in
        Wr.map2 write_addresses Wr.port_widths ~f:(fun write_entry_addr port_width ->
            let enable =
              write_enable.value &: (write_address.value ==:. write_entry_addr)
            in
            reg spec ~enable (uresize write_data.value port_width))
      in
      { O.slave_to_master = Slave_to_master.map ~f:Always.Variable.value slave_to_master
      ; write_values
      }
    ;;

    let hierarchical scope i =
      let module H = Hierarchy.In_scope (I) (O) in
      H.hierarchical ~name:"register_bank" ~scope create i
    ;;
  end
end
