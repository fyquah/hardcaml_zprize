open! Core
open Hardcaml
open! Hardcaml_waveterm
module Gf = Hardcaml_ntt.Gf

module Make (Config : Hardcaml_ntt.Core_config.S) = struct
  open Config
  module Kernel = Zprize_ntt.For_vitis.Make (Config)
  module Reference_model = Hardcaml_ntt.Reference_model.Make (Gf.Z)
  module Test_top = Test_top.Make (Config)
  module Sim = Cyclesim.With_interface (Kernel.I) (Kernel.O)
  module VSim = Hardcaml_verilator.With_interface (Kernel.I) (Kernel.O)

  (* Derived parameters *)
  let n = 1 lsl logn
  let num_cores = 1 lsl logcores
  let log_passes = logn - logcores
  let num_passes = 1 lsl log_passes
  let () = assert (1 lsl (logn + logn) = n * num_cores * num_passes)

  (* Common functions *)
  let random_input_coef_matrix = Test_top.random_input_coef_matrix
  let print_matrix = Test_top.print_matrix
  let copy_matrix = Test_top.copy_matrix
  let get_results = Test_top.get_results
  let transpose = Reference_model.transpose

  let create_sim ~verilator waves =
    let sim =
      if verilator
      then (
        let cache_dir = Sys.getenv_exn "HOME" ^/ ".hardcaml-verilator-cache" in
        VSim.create
          ~cache_dir
          ~verbose:true
          ~clock_names:[ "ap_clk" ]
          (Kernel.create
             ~build_mode:Simulation
             (Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true ())))
      else
        Sim.create
          ~config:Cyclesim.Config.trace_all
          (Kernel.create
             ~build_mode:Simulation
             (Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true ()))
    in
    let inputs = Cyclesim.inputs sim in
    let outputs = Cyclesim.outputs sim in
    let waves, sim =
      if waves
      then (
        let waves, sim = Waveform.create sim in
        Some waves, sim)
      else None, sim
    in
    sim, waves, inputs, outputs
  ;;

  let start_sim (inputs : _ Kernel.I.t) cycle =
    inputs.ap_rst_n := Bits.gnd;
    cycle ();
    inputs.ap_rst_n := Bits.vdd;
    inputs.compute_to_controller_dest.tready := Bits.vdd;
    cycle ();
    cycle ()
  ;;

  (* Perform the reference intt by using a standard full size single pass,
     rather than the 4step algorithm the hw uses. *)
  let reference_intt coefs =
    let coefs = Array.concat (Array.to_list (Array.copy coefs)) in
    Reference_model.inverse_dit coefs;
    Array.init n ~f:(fun row -> Array.init n ~f:(fun col -> coefs.((row * n) + col)))
  ;;

  let expected ~verbose input_coefs hw_results =
    let sw_results = reference_intt input_coefs in
    if verbose
    then
      List.iter
        [ "inputs", input_coefs; "sw", sw_results; "hw", hw_results ]
        ~f:(fun (n, m) ->
          printf "\n%s\n\n" n;
          print_matrix m);
    if [%equal: Gf.Z.t array array] hw_results sw_results
    then print_s [%message "Hardware and software reference results match!"]
    else raise_s [%message "ERROR: Hardware and software results do not match :("]
  ;;

  let run
    ?(verbose = false)
    ?(waves = false)
    ?(verilator = false)
    (input_coefs : Z.t array array)
    =
    let sim, waves, inputs, outputs = create_sim ~verilator waves in
    let input_coefs = Array.map input_coefs ~f:(Array.map ~f:Gf.Z.of_z) in
    let results = ref [] in
    let num_results = ref 0 in
    let cycle ?(n = 1) () =
      assert (n > 0);
      for _ = 1 to n do
        if Bits.to_bool !(outputs.compute_to_controller.tvalid)
        then (
          results := !(outputs.compute_to_controller.tdata) :: !results;
          Int.incr num_results);
        Cyclesim.cycle sim
      done
    in
    start_sim inputs cycle;
    let run_pass ~which coefs =
      let controller_to_compute =
        match which with
        | `First -> inputs.controller_to_compute_phase_1
        | `Second -> inputs.controller_to_compute_phase_2
      in
      let controller_to_compute_dest =
        match which with
        | `First -> outputs.controller_to_compute_phase_1_dest
        | `Second -> outputs.controller_to_compute_phase_2_dest
      in
      num_results := 0;
      results := [];
      (match which with
       | `First ->
         (* cheat - force the core to [start] *)
         controller_to_compute.tvalid := Bits.vdd;
         cycle ();
         (* wait for tready *)
         assert (Bits.to_bool !(controller_to_compute_dest.tready));
         for pass = 0 to num_passes - 1 do
           for i = 0 to n - 1 do
             controller_to_compute.tvalid := Bits.vdd;
             controller_to_compute.tdata
               := List.init num_cores ~f:(fun core ->
                    coefs.((pass * num_cores) + core).(i))
                  |> List.map ~f:(fun z -> Gf.Bits.to_bits (Gf.Bits.of_z (Gf.Z.to_z z)))
                  |> Bits.concat_lsb;
             while Bits.is_gnd !(controller_to_compute_dest.tready) do
               cycle ()
             done;
             cycle ()
           done;
           controller_to_compute.tvalid := Bits.gnd;
           (* assert (not (Bits.to_bool !(controller_to_compute_dest.tready))) *)
           (* A few cycles of flushing after each pass *)
           cycle ~n:4 ()
         done
       | `Second ->
         while not (Bits.to_bool !(controller_to_compute_dest.tready)) do
           cycle ()
         done;
         for i = 0 to (n / 8) - 1 do
           for j = 0 to (n / 8) - 1 do
             for k = 0 to 8 - 1 do
               let indices = List.init 8 ~f:(fun l -> (8 * i) + k, (8 * j) + l) in
               controller_to_compute.tvalid := Bits.vdd;
               controller_to_compute.tdata
                 := List.map indices ~f:(fun (r, c) -> coefs.(r).(c))
                    |> List.map ~f:(fun z -> Gf.Bits.to_bits (Gf.Bits.of_z (Gf.Z.to_z z)))
                    |> Bits.concat_lsb;
               while Bits.is_gnd !(controller_to_compute_dest.tready) do
                 cycle ()
               done;
               cycle ()
             done
           done
         done;
         controller_to_compute.tvalid := Bits.gnd;
         (* A few cycles of flushing after each pass *)
         cycle ~n:4 ());
      (* wait for the core to return all results. *)
      while !num_results <> n * n / num_cores do
        cycle ()
      done;
      get_results !results
    in
    (try
       let pass1 = run_pass ~which:`First (transpose input_coefs) in
       let pass2 = transpose (run_pass ~which:`Second (transpose pass1)) in
       cycle ~n:4 ();
       expected ~verbose input_coefs pass2
     with
     | e -> print_s [%message "RAISED :(" (e : exn)]);
    waves
  ;;
end

let%expect_test "vitis kernel test" =
  let module Config = struct
    let logn = 5
    let logcores = 3
    let logblocks = 0
    let support_4step_twiddle = true
  end
  in
  let module Test = Make (Config) in
  let input_coefs = Test.random_input_coef_matrix () in
  ignore
    (Test.run ~verilator:false ~verbose:false ~waves:false input_coefs
      : Waveform.t option);
  [%expect {|
    "Hardware and software reference results match!" |}]
;;

let%expect_test "2 blocks" =
  let module Config = struct
    let logn = 5
    let logcores = 3
    let logblocks = 1
    let support_4step_twiddle = true
  end
  in
  let module Test = Make (Config) in
  let input_coefs = Test.random_input_coef_matrix () in
  ignore
    (Test.run ~verilator:false ~verbose:false ~waves:false input_coefs
      : Waveform.t option);
  [%expect {|
    "Hardware and software reference results match!" |}]
;;

(* Thinking out aloud.

We are now aiming for memory bandwidth optimisation on reads and writes.

The biggest thing we can current do, is try to burst as much as possible from host memory.

We are contrained by needing to read/write in both linear and transposed orders (there
is a transposer component included in the hardware design)

We also have 8 seperate memory ports to the individual NTT rams.

The steps are:

1.  Given data layed out in linear order.
2.  READ into NTT in transposed order.
3.  WRITE from NTT in linear order
4.  READ into NTT in transposed order
5.  WRITE from NTT in transposed order

(note; linear/transposed orders in steps 3 and 4 can be swapped.)

Lets consider each in turn.Arith_status

READ transposed
===============

Read 8 coefs, write directly into the cores, stride += n

With more cores, we can read 64 coefs, write directly, stride += n

WRITE transpsosed
=================

Write 8 coefs (1 from each core), stride += n

Similar story as read for more cores

(is this actually right?)

READ linear
===========

Read into transposer component.  Same access pattern as read transposed.

WRITE linear
============

Not actually used ... 

Needs the transposer, just like read linear.

*)

let read_words ~log_cores inputs ~row ~col =
  Array.init (1 lsl log_cores) ~f:(fun c -> inputs.(row).(col + c))
;;

let read_block ~log_cores ~log_blocks ~block_row ~block_col (inputs : 'a array array) =
  let log_total = log_blocks + log_cores in
  for row = 0 to (1 lsl log_total) - 1 do
    for col = 0 to (1 lsl log_total) - 1 do
      ()
    done
  done
;;

(* Assume we are writing 8 coefs per cycle, and we have [1<<(log_blocks+log_cores)] cores *)
let read_transposed ~log_cores ~log_blocks (inputs : 'a array array) =
  (* array_ordering.(row).(col) *)
  let n = Array.length inputs in
  let log_total = log_cores + log_blocks in
  for col = 0 to (n lsr log_total) - 1 do
    for row = 0 to n - 1 do
      for word = 0 to (1 lsl log_blocks) - 1 do
        let x =
          read_words
            ~log_cores
            inputs
            ~row
            ~col:((col lsl log_total) + (word lsl log_cores))
        in
        print_s [%message "" ~_:(x : int array)]
      done
    done
  done
;;

let create_inputs n =
  Array.init n ~f:(fun row -> Array.init n ~f:(fun col -> (row * n) + col))
;;

let%expect_test "8 word transfers within 16x16" =
  read_transposed ~log_cores:3 ~log_blocks:0 (create_inputs 16);
  [%expect
    {|
    (0 1 2 3 4 5 6 7)
    (16 17 18 19 20 21 22 23)
    (32 33 34 35 36 37 38 39)
    (48 49 50 51 52 53 54 55)
    (64 65 66 67 68 69 70 71)
    (80 81 82 83 84 85 86 87)
    (96 97 98 99 100 101 102 103)
    (112 113 114 115 116 117 118 119)
    (128 129 130 131 132 133 134 135)
    (144 145 146 147 148 149 150 151)
    (160 161 162 163 164 165 166 167)
    (176 177 178 179 180 181 182 183)
    (192 193 194 195 196 197 198 199)
    (208 209 210 211 212 213 214 215)
    (224 225 226 227 228 229 230 231)
    (240 241 242 243 244 245 246 247)
    (8 9 10 11 12 13 14 15)
    (24 25 26 27 28 29 30 31)
    (40 41 42 43 44 45 46 47)
    (56 57 58 59 60 61 62 63)
    (72 73 74 75 76 77 78 79)
    (88 89 90 91 92 93 94 95)
    (104 105 106 107 108 109 110 111)
    (120 121 122 123 124 125 126 127)
    (136 137 138 139 140 141 142 143)
    (152 153 154 155 156 157 158 159)
    (168 169 170 171 172 173 174 175)
    (184 185 186 187 188 189 190 191)
    (200 201 202 203 204 205 206 207)
    (216 217 218 219 220 221 222 223)
    (232 233 234 235 236 237 238 239)
    (248 249 250 251 252 253 254 255) |}];
  read_transposed ~log_cores:2 ~log_blocks:1 (create_inputs 16);
  [%expect
    {|
    (0 1 2 3)
    (4 5 6 7)
    (16 17 18 19)
    (20 21 22 23)
    (32 33 34 35)
    (36 37 38 39)
    (48 49 50 51)
    (52 53 54 55)
    (64 65 66 67)
    (68 69 70 71)
    (80 81 82 83)
    (84 85 86 87)
    (96 97 98 99)
    (100 101 102 103)
    (112 113 114 115)
    (116 117 118 119)
    (128 129 130 131)
    (132 133 134 135)
    (144 145 146 147)
    (148 149 150 151)
    (160 161 162 163)
    (164 165 166 167)
    (176 177 178 179)
    (180 181 182 183)
    (192 193 194 195)
    (196 197 198 199)
    (208 209 210 211)
    (212 213 214 215)
    (224 225 226 227)
    (228 229 230 231)
    (240 241 242 243)
    (244 245 246 247)
    (8 9 10 11)
    (12 13 14 15)
    (24 25 26 27)
    (28 29 30 31)
    (40 41 42 43)
    (44 45 46 47)
    (56 57 58 59)
    (60 61 62 63)
    (72 73 74 75)
    (76 77 78 79)
    (88 89 90 91)
    (92 93 94 95)
    (104 105 106 107)
    (108 109 110 111)
    (120 121 122 123)
    (124 125 126 127)
    (136 137 138 139)
    (140 141 142 143)
    (152 153 154 155)
    (156 157 158 159)
    (168 169 170 171)
    (172 173 174 175)
    (184 185 186 187)
    (188 189 190 191)
    (200 201 202 203)
    (204 205 206 207)
    (216 217 218 219)
    (220 221 222 223)
    (232 233 234 235)
    (236 237 238 239)
    (248 249 250 251)
    (252 253 254 255) |}]
;;

let%expect_test "2 word transfers within 4x4" =
  read_transposed ~log_cores:1 ~log_blocks:0 (create_inputs 4);
  [%expect
    {|
    (0 1)
    (4 5)
    (8 9)
    (12 13)
    (2 3)
    (6 7)
    (10 11)
    (14 15) |}];
  read_transposed ~log_cores:1 ~log_blocks:1 (create_inputs 4);
  [%expect
    {|
    (0 1)
    (2 3)
    (4 5)
    (6 7)
    (8 9)
    (10 11)
    (12 13)
    (14 15) |}]
;;

let%expect_test "2 word transfers within 8x8" =
  read_transposed ~log_cores:1 ~log_blocks:0 (create_inputs 8);
  [%expect
    {|
    (0 1)
    (8 9)
    (16 17)
    (24 25)
    (32 33)
    (40 41)
    (48 49)
    (56 57)
    (2 3)
    (10 11)
    (18 19)
    (26 27)
    (34 35)
    (42 43)
    (50 51)
    (58 59)
    (4 5)
    (12 13)
    (20 21)
    (28 29)
    (36 37)
    (44 45)
    (52 53)
    (60 61)
    (6 7)
    (14 15)
    (22 23)
    (30 31)
    (38 39)
    (46 47)
    (54 55)
    (62 63) |}];
  read_transposed ~log_cores:1 ~log_blocks:1 (create_inputs 8);
  [%expect
    {|
    (0 1)
    (2 3)
    (8 9)
    (10 11)
    (16 17)
    (18 19)
    (24 25)
    (26 27)
    (32 33)
    (34 35)
    (40 41)
    (42 43)
    (48 49)
    (50 51)
    (56 57)
    (58 59)
    (4 5)
    (6 7)
    (12 13)
    (14 15)
    (20 21)
    (22 23)
    (28 29)
    (30 31)
    (36 37)
    (38 39)
    (44 45)
    (46 47)
    (52 53)
    (54 55)
    (60 61)
    (62 63) |}];
  read_transposed ~log_cores:1 ~log_blocks:2 (create_inputs 8);
  [%expect
    {|
    (0 1)
    (2 3)
    (4 5)
    (6 7)
    (8 9)
    (10 11)
    (12 13)
    (14 15)
    (16 17)
    (18 19)
    (20 21)
    (22 23)
    (24 25)
    (26 27)
    (28 29)
    (30 31)
    (32 33)
    (34 35)
    (36 37)
    (38 39)
    (40 41)
    (42 43)
    (44 45)
    (46 47)
    (48 49)
    (50 51)
    (52 53)
    (54 55)
    (56 57)
    (58 59)
    (60 61)
    (62 63) |}]
;;
