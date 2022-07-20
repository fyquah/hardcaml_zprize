module Make (Gf : Gf_intf.S) : sig
  (** Standard decimation in time algorithm *)
  val dit : Gf.t array -> unit

  (** Standard decimation in frequency algorithm *)
  val dif : Gf.t array -> unit

  val matrix : Gf.t array -> int -> int -> Gf.t array array
  val transpose : Gf.t array array -> Gf.t array array
  val row : Gf.t array array -> int -> Gf.t array
  val col : Gf.t array array -> int -> Gf.t array

  (** Four step decomposition *)
  val four_step : Gf.t array -> int -> Gf.t array
end
