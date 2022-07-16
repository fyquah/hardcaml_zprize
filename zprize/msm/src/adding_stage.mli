open Hardcaml

module Make (Config : Config.S) (Stage_interfaces : Stage_interfaces.M(Config).S) : sig
  module Stage_input := Stage_interfaces.Adding.Stage_input
  module Stage_output := Stage_interfaces.Adding.Stage_output

  val latency : int

  val create
    :  clock:Signal.t
    -> scope:Scope.t
    -> Signal.t Stage_input.t
    -> Signal.t Stage_output.t
end
