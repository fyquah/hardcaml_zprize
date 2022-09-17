module Hardcaml_axi_stream = Stream
open Base
open Hardcaml
open Signal

module type Config = sig
  val num_outputs : int
end

module Make (Stream : Hardcaml_axi_stream.S) (Config : Config) = struct
  open Config

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; up : 'a Stream.Source.t [@rtlprefix "up$"]
      ; dn_dests : 'a Stream.Dest.t array [@length num_outputs] [@rtlprefix "dn$"]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { dns : 'a Stream.Source.t array [@length num_outputs] [@rtlprefix "dn$"]
      ; up_dest : 'a Stream.Dest.t [@rtlprefix "up$"]
      }
    [@@deriving sexp_of, hardcaml]
  end

  (* This is not necessary the most tming efficient, but it prevents making
   * a 512-bit register buffering. That decision is intended be left to the
   * caller whether to insert a skid buffer.
   *)
  let create (_ : Scope.t) { I.clock; clear; up; dn_dests } =
    let spec = Reg_spec.create ~clock ~clear () in
    let up_tready = wire 1 in
    let dn_tvalids = Array.map dn_dests ~f:(fun _ -> Signal.wire 1) in
    let processed_this_cycle =
      Array.map2_exn dn_tvalids dn_dests ~f:(fun dn_tvalid dn_dest ->
        let processed_this_cycle =
          reg_fb ~width:1 spec ~f:(fun fb ->
            mux2 up_tready gnd @@ mux2 (dn_tvalid &: dn_dest.tready) vdd @@ fb)
        in
        dn_tvalid <== (~:processed_this_cycle &: up.tvalid);
        processed_this_cycle)
    in
    let dns = Array.map dn_tvalids ~f:(fun tvalid -> { up with tvalid }) in
    up_tready
    <== (Array.map2_exn processed_this_cycle dn_dests ~f:(fun p x -> p |: x.tready)
        |> Array.to_list
        |> Signal.reduce ~f:( &: ));
    { O.dns; up_dest = { tready = up_tready } }
  ;;
end
