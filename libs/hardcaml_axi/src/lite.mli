open Hardcaml

module Make (_ : Config.S) : sig
  module Master_to_slave : sig
    type 'a t =
      { awaddr : 'a
      ; awvalid : 'a
      ; wdata : 'a
      ; wstrb : 'a
      ; wvalid : 'a
      ; bready : 'a
      ; araddr : 'a
      ; arvalid : 'a
      ; rready : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module Slave_to_master : sig
    type 'a t =
      { awready : 'a
      ; wready : 'a
      ; bresp : 'a
      ; bvalid : 'a
      ; arready : 'a
      ; rdata : 'a
      ; rresp : 'a
      ; rvalid : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module Register_bank (Rd : Hardcaml.Interface.S) (Wr : Hardcaml.Interface.S) : sig
    module I : sig
      type 'a t =
        { clock : 'a
        ; clear : 'a
        ; master_to_slave : 'a Master_to_slave.t
        ; read_values : 'a Rd.t
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O : sig
      type 'a t =
        { slave_to_master : 'a Slave_to_master.t
        ; write_values : 'a Wr.t
        }
      [@@deriving sexp_of, hardcaml]
    end

    val create : Scope.t -> Signal.t I.t -> Signal.t O.t
    val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t
  end
end
