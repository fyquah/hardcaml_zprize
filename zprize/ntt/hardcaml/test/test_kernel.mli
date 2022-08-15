module Make (Size : sig
  val logn : int
end) : sig
  val random_input_coefs : unit -> Z.t list
  val run : ?verbose:bool -> Z.t list -> Hardcaml_waveterm.Waveform.t
end
