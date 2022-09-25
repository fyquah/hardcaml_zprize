(** Converts the scalars to a window-compressed form. *)

open Core
open Hardcaml
open Signal

module Make (Config : Config.S) = struct
  module Host_to_msm = Host_to_msm.Make (Config)
  open Config_utils.Make (Config)

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; host_to_msm : 'a Host_to_msm.O.t
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t = { host_to_msm : 'a Host_to_msm.O.t } [@@deriving sexp_of, hardcaml]
  end

  let latency = num_windows - 1

  let create _scope (i : _ I.t) : _ O.t =
    (*let ( -- ) = Scope.naming scope in*)
    let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let open Always in
    (* construct the pipeline *)
    let module Window = struct
      type t =
        { bucket : Variable.t
        ; carry : Variable.t
        }
    end
    in
    let pipeline_stages =
      Array.init num_windows ~f:(fun stage ->
        Array.map window_bit_sizes ~f:(fun width ->
          if stage = 0
          then
            { Window.bucket = Variable.wire ~default:(zero width)
            ; carry = Variable.wire ~default:(zero 2)
            }
          else
            { Window.bucket = Variable.reg spec ~width
            ; carry = Variable.reg spec ~width:2
            }))
    in
    (* assign input to the first stage *)
    let assign_inputs_to_pipeline =
      let rec compute_offsets i data cur_offset =
        if i = num_windows
        then data
        else
          compute_offsets (i + 1) (cur_offset :: data) (cur_offset + window_bit_sizes.(i))
      in
      let window_bit_offsets = compute_offsets 0 [] 0 |> Array.of_list in
      Array.map2_exn
        pipeline_stages.(0)
        window_bit_offsets
        ~f:(fun { bucket; carry } offset ->
        let width = Some (width bucket.value) in
        [ bucket <-- i.host_to_msm.scalar.:+[offset, width]; carry <--. 0 ] |> proc)
      |> Array.to_list
      |> proc
    in
    let create_pipeline_stage i =
      (* stage 0 is just the input *)
      assert (0 < i && i < num_windows);
      let c_cur = width pipeline_stages.(i).(i - 1).bucket.value in
      let c_next = width pipeline_stages.(i).(i).bucket.value in
      assert (c_next - c_cur > width pipeline_stages.(i).(i).carry.value);
      (* first perform the carry *)
      let cur_bucket_with_carry =
        Uop.(
          pipeline_stages.(i - 1).(i - 1).bucket.value
          +: pipeline_stages.(i - 1).(i - 1).carry.value)
      in
      let cur_carry, cur_bucket = msb cur_bucket_with_carry, lsbs cur_bucket_with_carry in
      (* if x_{i-1} >= 2^{c-1}, then use (x_{c_1} - 2^c) instead *)
      let shift = msb cur_bucket in
      [ (* default values for this stage *)
        Array.map2_exn
          pipeline_stages.(i)
          pipeline_stages.(i - 1)
          ~f:(fun { bucket; carry } { bucket = prev_bucket; _ } ->
            [ bucket <-- prev_bucket.value; carry <--. 0 ])
        |> Array.to_list
        |> List.concat
        |> proc
      ; if_
          cur_carry
          [ (* the current stage overflowed, we assume that this implies the bucket is <2^{c-1} *)
            pipeline_stages.(i).(i).carry <--. 1 lsl (c_next - c_cur)
          ]
          [ (* there was no overflow, check for the shift *)
            if_
              shift
              [ pipeline_stages.(i).(i - 1).bucket
                <-- sel_bottom
                      Sop.(cur_bucket -: of_int (1 lsl c_cur) ~width:(c_cur + 1))
                      c_cur
              ; pipeline_stages.(i).(i).carry <--. 1 lsl (c_next - c_cur)
              ]
              [ pipeline_stages.(i).(i - 1).bucket <-- cur_bucket ]
          ]
      ]
      |> proc
    in
    compile
      (assign_inputs_to_pipeline
      :: List.init (num_windows - 1) ~f:(fun i -> create_pipeline_stage (i + 1)));
    (* construct the output *)
    let pipelined_input = Host_to_msm.O.map i.host_to_msm ~f:(pipeline spec ~n:latency) in
    let output_scalar =
      Array.mapi
        pipeline_stages.(num_windows - 1)
        ~f:(fun i v ->
          if i = num_windows - 1
          then (* this can never overflow *)
            v.bucket.value +: v.carry.value
          else v.bucket.value)
      |> Array.to_list
      |> concat_lsb
    in
    { O.host_to_msm = { pipelined_input with scalar = output_scalar } }
  ;;

  let hierarchical ?instance scope =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ?instance ~name:"scalar_transformation" ~scope create
  ;;
end
