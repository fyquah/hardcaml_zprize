module For_bls12_377 : sig
  val montgomery_reduction_config : Montgomery_reduction.Config.t
  val barrett_reduction_config : Barrett_reduction.Config.t
  val point_double_with_montgomery_reduction : Ec_fpn_dbl.Config.t
  val point_double_with_barrett_reduction : Ec_fpn_dbl.Config.t
end
