module Make (Config : Ntts_r_fun.Ntt.Config) : sig
  val random_input_coefs : unit -> Z.t list
  val run : ?verbose:bool -> Z.t list -> Hardcaml_waveterm.Waveform.t
end
