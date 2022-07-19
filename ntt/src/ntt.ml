(* *)
open! Base
open! Hardcaml

(* Max transform size*)
let n = 8
let logn = Int.ceil_log2 n

module Gf = Gf_bits.Make (Hardcaml.Signal)

module Controller = struct
  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; start : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { done_ : 'a
      ; i : 'a [@bits Int.ceil_log2 logn]
      ; j : 'a [@bits logn]
      ; k : 'a [@bits logn]
      ; m : 'a [@bits logn]
      ; addr1 : 'a [@bits logn]
      ; addr2 : 'a [@bits logn]
      ; omega : 'a [@bits Gf.num_bits]
      ; start_twiddles : 'a
      ; first_stage : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module State = struct
    type t =
      | Idle
      | Looping
    [@@deriving compare, enumerate, sexp_of, variants]
  end

  module Var = Always.Variable

  let create _scope (inputs : _ I.t) =
    let open Signal in
    let spec = Reg_spec.create ~clock:inputs.clock ~clear:inputs.clear () in
    let sm = Always.State_machine.create (module State) spec in
    let done_ = Var.reg (Reg_spec.override spec ~clear_to:vdd) ~width:1 in
    let i = Var.reg spec ~width:(Int.ceil_log2 (logn + 1)) in
    let i_next = i.value +:. 1 in
    let j = Var.reg spec ~width:logn in
    let j_next = j.value +:. 1 in
    let k = Var.reg spec ~width:logn in
    let m = Var.reg spec ~width:logn in
    let m_next = sll m.value 1 in
    let k_next = k.value +: m_next in
    let addr1 = Var.reg spec ~width:logn in
    let addr2 = Var.reg spec ~width:logn in
    let omega = List.init logn ~f:(fun i -> Gf.to_bits Gf.omega.(i + 1)) |> mux i.value in
    let start_twiddles = Var.reg spec ~width:1 in
    let first_stage = Var.reg spec ~width:1 in
    Always.(
      compile
        [ start_twiddles <--. 0
        ; sm.switch
            [ ( Idle
              , [ when_
                    inputs.start
                    [ i <--. 0
                    ; j <--. 0
                    ; k <--. 0
                    ; m <--. 1
                    ; addr1 <--. 0
                    ; addr2 <--. 1
                    ; done_ <--. 0
                    ; start_twiddles <--. 1
                    ; first_stage <--. 1
                    ; sm.set_next Looping
                    ]
                ] )
            ; ( Looping
              , [ j <-- j_next
                ; addr1 <-- addr1.value +:. 1
                ; addr2 <-- addr2.value +:. 1
                ; (* perform the butterfly here.*)
                  when_
                    (j_next ==: m.value)
                    [ j <--. 0
                    ; start_twiddles <--. 1
                    ; k <-- k_next
                    ; addr1 <-- k_next
                    ; addr2 <-- k_next +: m.value
                    ; when_
                        (k_next ==:. 0)
                        [ i <-- i_next
                        ; first_stage <--. 0
                        ; m <-- m_next
                        ; addr2 <-- k_next +: m_next
                        ; when_ (i_next ==:. logn) [ done_ <--. 1; sm.set_next Idle ]
                        ]
                    ]
                ] )
            ]
        ]);
    { O.done_ = done_.value
    ; i = i.value
    ; j = j.value
    ; k = k.value
    ; m = m.value
    ; addr1 = addr1.value
    ; addr2 = addr2.value
    ; omega
    ; start_twiddles = start_twiddles.value
    ; first_stage = first_stage.value
    }
  ;;

  let hierarchy scope =
    let module Hier = Hierarchy.In_scope (I) (O) in
    Hier.hierarchical ~name:"ctrl" ~scope create
  ;;
end

(* Butterly and twiddle factor calculation *)
module Datapath = struct
  open Signal

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; d1 : 'a [@bits Gf.num_bits]
      ; d2 : 'a [@bits Gf.num_bits]
      ; omega : 'a [@bits Gf.num_bits]
      ; start_twiddles : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { q1 : 'a [@bits Gf.num_bits]
      ; q2 : 'a [@bits Gf.num_bits]
      }
    [@@deriving sexp_of, hardcaml]
  end

  let ( *: ) a b = Gf.( *: ) (Gf.of_bits a) (Gf.of_bits b) |> Gf.to_bits
  let ( +: ) a b = Gf.( +: ) (Gf.of_bits a) (Gf.of_bits b) |> Gf.to_bits
  let ( -: ) a b = Gf.( -: ) (Gf.of_bits a) (Gf.of_bits b) |> Gf.to_bits
  let twiddle_factor (i : _ I.t) w = mux2 i.start_twiddles Gf.(to_bits one) (w *: i.omega)

  let create _scope (i : _ I.t) =
    let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    (* the latency of the input data must be adjusted to match the latency of the twiddle factor calculation *)
    (* XXX aray: Need to up the latency for a practical version *)
    (* XXX aray: THe pipeline below is timed for 1 cycle ram latency, and 0 cycles twiddle latency *)
    let w = wire Gf.num_bits -- "twiddle" in
    w <== reg spec (twiddle_factor i w);
    let t = i.d2 *: w in
    { O.q1 = i.d1 +: t; q2 = i.d1 -: t }
  ;;

  let hierarchy scope =
    let module Hier = Hierarchy.In_scope (I) (O) in
    Hier.hierarchical ~name:"dp" ~scope create
  ;;
end

module Core = struct
  open! Signal

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; start : 'a
      ; d1 : 'a [@bits Gf.num_bits]
      ; d2 : 'a [@bits Gf.num_bits]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { q1 : 'a [@bits Gf.num_bits]
      ; q2 : 'a [@bits Gf.num_bits]
      ; addr1_in : 'a [@bits logn]
      ; addr2_in : 'a [@bits logn]
      ; read_enable_in : 'a
      ; addr1_out : 'a [@bits logn]
      ; addr2_out : 'a [@bits logn]
      ; write_enable_out : 'a
      ; first_stage : 'a
      ; done_ : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create scope (i : _ I.t) =
    let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let controller =
      Controller.hierarchy
        scope
        { Controller.I.clock = i.clock; clear = i.clear; start = i.start }
    in
    let datapath =
      Datapath.hierarchy
        scope
        { Datapath.I.clock = i.clock
        ; clear = i.clear
        ; d1 = i.d1
        ; d2 = i.d2
        ; omega = controller.omega
        ; start_twiddles = controller.start_twiddles
        }
    in
    let pipe = pipeline spec ~n:1 in
    { O.q1 = datapath.q1
    ; q2 = datapath.q2
    ; addr1_in = controller.addr1
    ; addr2_in = controller.addr2
    ; read_enable_in = ~:(controller.done_)
    ; addr1_out = controller.addr1 |> pipe
    ; addr2_out = controller.addr2 |> pipe
    ; write_enable_out = ~:(controller.done_) |> pipe
    ; first_stage = controller.first_stage
    ; done_ = controller.done_
    }
  ;;

  let hierarchy scope =
    let module Hier = Hierarchy.In_scope (I) (O) in
    Hier.hierarchical ~name:"core" ~scope create
  ;;
end

module With_rams = struct
  open! Signal

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; start : 'a
      ; wr_d : 'a [@bits Gf.num_bits]
      ; wr_en : 'a
      ; wr_addr : 'a [@bits logn]
      ; rd_en : 'a
      ; rd_addr : 'a [@bits logn]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { done_ : 'a
      ; rd_q : 'a [@bits Gf.num_bits]
      }
    [@@deriving sexp_of, hardcaml]
  end

  let input_ram (i : _ I.t) (core : _ Core.O.t) =
    let q =
      Ram.create
        ~name:"input_ram"
        ~collision_mode:Write_before_read
        ~size:n
        ~write_ports:
          [| { write_clock = i.clock
             ; write_address = i.wr_addr
             ; write_data = i.wr_d
             ; write_enable = i.wr_en
             }
          |]
        ~read_ports:
          [| { read_clock = i.clock
             ; read_address = reverse core.addr1_in
             ; read_enable = core.read_enable_in
             }
           ; { read_clock = i.clock
             ; read_address = reverse core.addr2_in
             ; read_enable = core.read_enable_in
             }
          |]
        ()
    in
    q
  ;;

  let output_ram (i : _ I.t) (core : _ Core.O.t) =
    let q =
      Ram.create
        ~name:"output_ram"
        ~collision_mode:Write_before_read
        ~size:n
        ~write_ports:
          [| { write_clock = i.clock
             ; write_address = core.addr1_out
             ; write_data = core.q1
             ; write_enable = core.write_enable_out
             }
           ; { write_clock = i.clock
             ; write_address = core.addr2_out
             ; write_data = core.q2
             ; write_enable = core.write_enable_out
             }
          |]
        ~read_ports:
          [| { read_clock = i.clock
             ; read_address = core.addr1_in
             ; read_enable = core.read_enable_in
             }
           ; { read_clock = i.clock
             ; read_address = core.addr2_in
             ; read_enable = core.read_enable_in
             }
           ; { read_clock = i.clock; read_address = i.rd_addr; read_enable = i.rd_en }
          |]
        ()
    in
    q
  ;;

  let create scope (i : _ I.t) =
    let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let core = Core.O.Of_signal.wires () in
    (* input and output rams *)
    let d_in = input_ram i core in
    let d_out = output_ram i core in
    (* core *)
    let piped_first_stage = pipeline spec ~n:1 core.first_stage in
    Core.O.iter2
      core
      (Core.hierarchy
         scope
         { clock = i.clock
         ; clear = i.clear
         ; start = i.start
         ; d1 = mux2 piped_first_stage d_in.(0) d_out.(0)
         ; d2 = mux2 piped_first_stage d_in.(1) d_out.(1)
         })
      ~f:( <== );
    { O.done_ = core.done_; rd_q = d_out.(2) }
  ;;

  let hierarchy scope =
    let module Hier = Hierarchy.In_scope (I) (O) in
    Hier.hierarchical ~name:"top" ~scope create
  ;;
end

module Reference = struct
  open! Bits
  module Gf = Gf_bits.Make (Bits)

  let bit_reversed_addressing input =
    let logn = Int.ceil_log2 (Array.length input) in
    let max = ones logn in
    let rec loop k =
      let rk = reverse k in
      if Bits.equal (k <: rk) vdd
      then (
        let tmp = input.(to_int rk) in
        input.(to_int rk) <- input.(to_int k);
        input.(to_int k) <- tmp);
      if Bits.equal k max then () else loop (k +:. 1)
    in
    loop (zero logn)
  ;;

  let debugging = false

  let rec loop3 ~input ~i ~j ~k ~m ~w ~w_m =
    if j >= m
    then ()
    else (
      let t1 = input.(k + j) in
      let t2 = input.(k + j + m) in
      let t = Gf.(t2 *: w) in
      input.(k + j) <- Gf.(t1 +: t);
      input.(k + j + m) <- Gf.(t1 -: t);
      if debugging
      then
        Stdio.printf
          "%i %i %s %s %s %s\n"
          (k + j)
          (k + j + m)
          (Gf.to_z t1 |> Z.to_string)
          (Gf.to_z t2 |> Z.to_string)
          (Gf.to_z input.(k + j) |> Z.to_string)
          (Gf.to_z input.(k + j + m) |> Z.to_string);
      loop3 ~input ~i ~j:(j + 1) ~k ~m ~w:Gf.(w *: w_m) ~w_m)
  ;;

  let rec loop2 ~input ~i ~k ~m ~n ~w_m =
    if k >= n
    then ()
    else (
      loop3 ~input ~i ~j:0 ~k ~m ~w:Gf.one ~w_m;
      loop2 ~input ~i ~k:(k + (2 * m)) ~m ~n ~w_m)
  ;;

  let rec loop1 ~input ~logn ~i ~m =
    if i > logn
    then ()
    else (
      loop2 ~input ~i ~k:0 ~m ~n:(1 lsl logn) ~w_m:Gf.omega.(i);
      loop1 ~input ~logn ~i:(i + 1) ~m:(m * 2))
  ;;

  let ntt input =
    bit_reversed_addressing input;
    let logn = Int.ceil_log2 (Array.length input) in
    loop1 ~input ~logn ~i:1 ~m:1
  ;;

  let dit a =
    bit_reversed_addressing a;
    let n = Array.length a in
    let logn = Int.ceil_log2 n in
    for s = 1 to logn do
      let m = 1 lsl s in
      let wm = Gf.omega.(s) in
      let k = ref 0 in
      while !k < n do
        let w = ref Gf.one in
        for j = 0 to (m / 2) - 1 do
          let u = a.(!k + j) in
          let v = Gf.( *: ) !w a.(!k + j + (m / 2)) in
          a.(!k + j) <- Gf.(u +: v);
          a.(!k + j + (m / 2)) <- Gf.(u -: v);
          w := Gf.(!w *: wm)
        done;
        k := !k + m
      done
    done
  ;;

  let dif a =
    let n = Array.length a in
    let logn = Int.ceil_log2 n in
    for s = logn downto 1 do
      let m = 1 lsl s in
      let wm = Gf.omega.(s) in
      let k = ref 0 in
      while !k < n do
        let w = ref Gf.one in
        for j = 0 to (m / 2) - 1 do
          let u = a.(!k + j) in
          let v = a.(!k + j + (m / 2)) in
          a.(!k + j) <- Gf.(u +: v);
          a.(!k + j + (m / 2)) <- Gf.(!w *: (u -: v));
          w := Gf.(!w *: wm)
        done;
        k := !k + m
      done
    done;
    bit_reversed_addressing a
  ;;

  let matrix a log_rows log_cols =
    let rows = 1 lsl log_rows in
    let cols = 1 lsl log_cols in
    Array.init rows ~f:(fun row -> Array.init cols ~f:(fun col -> a.((row * cols) + col)))
  ;;

  let transpose a =
    let rows = Array.length a in
    let cols = Array.length a.(0) in
    Array.init cols ~f:(fun col -> Array.init rows ~f:(fun row -> a.(row).(col)))
  ;;

  let row a row = Array.copy a.(row)
  let col a col = Array.init (Array.length a) ~f:(fun row -> a.(row).(col))

  let apply_twiddles wm a =
    let rec row wm w a index =
      if index = Array.length a
      then ()
      else (
        a.(index) <- Gf.(w *: a.(index));
        row wm Gf.(wm *: w) a (index + 1))
    in
    let rec f wm w index =
      if index = Array.length a
      then ()
      else (
        row w Gf.one a.(index) 0;
        f wm Gf.(w *: wm) (index + 1))
    in
    f wm Gf.one 0
  ;;

  let four_step a log_rows =
    let n = Array.length a in
    let logn = Int.ceil_log2 n in
    let log_cols = logn - log_rows in
    assert (log_rows > 0);
    assert (log_cols > 0);
    let matrix = matrix a log_cols log_rows |> transpose in
    Array.iter matrix ~f:dit;
    apply_twiddles Gf.omega.(logn) matrix;
    let matrix = transpose matrix in
    Array.iter matrix ~f:dit;
    let matrix = transpose matrix in
    Array.concat (Array.to_list matrix)
  ;;
end
