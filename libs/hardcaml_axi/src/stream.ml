open Base
open Stream_intf

module type S = S

module Make (X : Config.S) = struct
  open X

  let tkeep_bits = data_bits / 8
  let tstrb_bits = data_bits / 8

  module Source = struct
    type 'a t =
      { tvalid : 'a
      ; tdata : 'a [@bits data_bits]
      ; tkeep : 'a [@bits tkeep_bits]
      ; tstrb : 'a [@bits tstrb_bits]
      ; tlast : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module Dest = struct
    type 'a t = { tready : 'a } [@@deriving sexp_of, hardcaml]
  end
end
