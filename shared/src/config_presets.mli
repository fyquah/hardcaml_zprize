module For_bls12_377 : sig
  val montgomery_reduction_config : Montgomery_reduction.Config.t
  val barrett_reduction_config : Barrett_reduction.Config.t
  val ec_fpn_ops_with_barrett_reduction : Ec_fpn_ops_config.t
  val ec_fpn_ops_with_montgomery_reduction : Ec_fpn_ops_config.t
end
