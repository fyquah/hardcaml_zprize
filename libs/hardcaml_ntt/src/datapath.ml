open! Base
open! Hardcaml

module Make (Config : Core_config.S) = struct
  open Config
  open Signal
  module Var = Always.Variable

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; start : 'a
      ; first_iter : 'a
      ; d1 : 'a [@bits Gf.Signal.num_bits]
      ; d2 : 'a [@bits Gf.Signal.num_bits]
      ; omegas : 'a list
           [@bits Gf.Signal.num_bits] [@length Twiddle_factor_stream.pipe_length]
      ; start_twiddles : 'a
      ; twiddle_stage : 'a
      ; twiddle_update : 'a Twiddle_update.t
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { q1 : 'a [@bits Gf.Signal.num_bits]
      ; q2 : 'a [@bits Gf.Signal.num_bits]
      ; twiddle_update_q : 'a [@bits Gf.Signal.num_bits]
      }
    [@@deriving sexp_of, hardcaml]
  end

  let support_4step_twiddle = Config.support_4step_twiddle
  let ram_latency = Core_config.ram_latency
  let ram_output_pipelining = Core_config.ram_output_pipelining
  let gf_z_to_bits g = Gf.Z.to_z g |> Gf.Signal.of_z |> Gf.Signal.to_bits
  let twiddle_root row iter = Gf.Z.pow Roots.inverse.(logn * 2) (row * (iter + 1))

  let twiddle_scale_z =
    List.init Twiddle_factor_stream.pipe_length ~f:(fun col ->
      twiddle_root (1 lsl (Config.logcores + Config.logblocks)) col)
  ;;

  let twiddle_scale = twiddle_scale_z |> List.map ~f:gf_z_to_bits

  let ( +: ) a b =
    Gf.Signal.( + ) (Gf.Signal.of_bits a) (Gf.Signal.of_bits b) |> Gf.Signal.to_bits
  ;;

  let ( -: ) a b =
    Gf.Signal.( - ) (Gf.Signal.of_bits a) (Gf.Signal.of_bits b) |> Gf.Signal.to_bits
  ;;

  let ( *: ) = `Dont_use_me
  let `Dont_use_me = ( *: )
  let for_ lo hi f = List.range lo (hi + 1) |> List.map ~f |> Always.proc

  let create ?(row = 0) scope (i : _ I.t) =
    let ( -- ) = Scope.naming scope in
    let spec_with_clear = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let spec_no_clear = Reg_spec.create ~clock:i.clock () in
    let pipe ?(clear = false) ~n d =
      pipeline ~n (if clear then spec_with_clear else spec_no_clear) d
    in
    let twiddle_omegas =
      Array.init Twiddle_factor_stream.pipe_length ~f:(fun _ ->
        Var.reg spec_no_clear ~width:Gf.Signal.num_bits)
    in
    (* The latency of the input data must be adjusted to match the latency of
         the twiddle factor calculation *)
    let { Twiddle_factor_stream.O.w } =
      Twiddle_factor_stream.hierarchy
        scope
        { clock = i.clock
        ; start_twiddles =
            (* -1 here because we need to make sure the twiddle factor
             * stream is ready for the next cycle when the ram data
             * is available.
             *)
            pipe ~n:(ram_latency + ram_output_pipelining - 1) ~clear:true i.start_twiddles
        ; omegas =
            (if support_4step_twiddle
            then
              List.init Twiddle_factor_stream.pipe_length ~f:(fun idx ->
                mux2
                  (pipe ~n:(ram_latency + ram_output_pipelining - 1) i.twiddle_stage)
                  twiddle_omegas.(idx).value
                  (List.nth_exn i.omegas idx))
            else i.omegas)
        }
    in
    Array.iteri twiddle_omegas ~f:(fun i t ->
      ignore (t.value -- ("twiddle_omega" ^ Int.to_string i) : Signal.t));
    let w = w -- "twiddle_factor" in
    let t = wire Gf.Signal.num_bits -- "T" in
    Always.(
      compile
        [ when_
            (i.start &: i.first_iter)
            [ for_ 0 (Twiddle_factor_stream.pipe_length - 1) (fun col ->
                twiddle_omegas.(col) <-- (twiddle_root row col |> gf_z_to_bits))
            ]
        ; when_
            (pipeline
               spec_with_clear
               ~n:
                 (Twiddle_factor_stream.pipe_length
                 + ram_output_pipelining
                 + ram_latency
                 - 1)
               i.twiddle_update.valid)
            [ (* depending on the pipe dpeth, this might not work.*)
              for_
                0
                (Array.length twiddle_omegas - 1)
                (fun i ->
                  if i = Array.length twiddle_omegas - 1
                  then twiddle_omegas.(i) <-- t
                  else twiddle_omegas.(i) <-- twiddle_omegas.(i + 1).value)
            ]
        ]);
    let () =
      let piped_twiddle_update_index =
        pipe ~n:(ram_latency + ram_output_pipelining) i.twiddle_update.index
      in
      let piped_twidle_updated_valid =
        pipe ~n:(ram_latency + ram_output_pipelining) i.twiddle_update.valid
        -- "piped_twidle_updated_valid"
      in
      let twiddle_omega =
        mux
          piped_twiddle_update_index
          (Array.to_list twiddle_omegas |> List.map ~f:(fun v -> v.value))
      in
      let twiddle_scale = mux piped_twiddle_update_index twiddle_scale in
      let a =
        mux2
          piped_twidle_updated_valid
          twiddle_omega
          (pipe ~n:ram_output_pipelining i.d2 -- "piped_d2")
        -- "a"
      in
      let b =
        mux2 piped_twidle_updated_valid (twiddle_scale -- "twiddle_scale") (w -- "w")
        -- "b"
      in
      t <== Multiplier.create ~clock:i.clock a b
    in
    let d1 = pipe ~n:(Multiplier.latency + ram_output_pipelining) i.d1 in
    let piped_twiddle_stage =
      pipe ~n:(Multiplier.latency + ram_output_pipelining + ram_latency) i.twiddle_stage
      -- "piped_twiddle_stage"
    in
    { O.q1 = reg spec_no_clear (d1 +: t)
    ; q2 = reg spec_no_clear (mux2 piped_twiddle_stage t (d1 -: t))
    ; twiddle_update_q = t
    }
  ;;

  let hierarchy ?row scope =
    let module Hier = Hierarchy.In_scope (I) (O) in
    Hier.hierarchical ~name:"dp" ~scope (create ?row)
  ;;
end
