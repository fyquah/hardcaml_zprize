open Core

module Accumulator = struct
  let create () = ref []
  let push l hd = l := hd :: !l
  let dump l = List.rev !l
end

let random_bool ~p_true = Float.(Random.float 1.0 < p_true)
