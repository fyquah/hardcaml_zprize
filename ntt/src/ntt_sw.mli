module Make (Gf : Gf_intf.S) : sig
  (** {2 Standard decimation in time algorithm} *)

  val forward_dit : Gf.t array -> unit
  val inverse_dit : Gf.t array -> unit

  (** {2 Standard decimation in frequency algorithm} *)

  val forward_dif : Gf.t array -> unit
  val inverse_dif : Gf.t array -> unit

  (** {2 Four step decomposition} *)

  val matrix : Gf.t array -> int -> int -> Gf.t array array
  val transpose : Gf.t array array -> Gf.t array array
  val row : Gf.t array array -> int -> Gf.t array
  val col : Gf.t array array -> int -> Gf.t array
  val four_step : Gf.t array -> int -> Gf.t array
end
