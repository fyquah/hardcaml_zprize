open Hardcaml

module Make (Config : Config.S) : sig
  module Host_to_msm : module type of Host_to_msm.Make (Config)

  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; host_to_msm : 'a Host_to_msm.O.t
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O : sig
    type 'a t = { host_to_msm : 'a Host_to_msm.O.t } [@@deriving sexp_of, hardcaml]
  end

  val hierarchical : ?instance:string -> Scope.t -> Signal.t Interface.Create_fn(I)(O).t
end
