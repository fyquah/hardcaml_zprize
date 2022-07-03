module type S = sig
  module Source : sig
    type 'a t =
      { tvalid : 'a
      ; tdata : 'a
      ; tkeep : 'a
      ; tstrb : 'a
      ; tlast : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module Dest : sig
    type 'a t = { tready : 'a } [@@deriving sexp_of, hardcaml]
  end
end

module type Stream = sig
  module type S = S

  module Make (_ : Config.S) : S
end
