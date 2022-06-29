(* *)
open! Base
open! Hardcaml

(* Max transform size*)
let n = 8
let logn = Int.ceil_log2 n

module Gfhw = Gf.Make (Hardcaml.Signal)

module Address_controller = struct
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
    let module Gf = Gf.Make (Signal) in
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
    let _omega =
      List.init logn ~f:(fun i -> Gf.to_bits Gf.omega.(i + 1))
      |> mux i.value
      |> Gf.of_bits
    in
    Always.(
      compile
        [ sm.switch
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
                    ; k <-- k_next
                    ; addr1 <-- k_next
                    ; addr2 <-- k_next +: m.value
                    ; when_
                        (k_next ==:. 0)
                        [ i <-- i_next
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
    }
  ;;
end

module Reference = struct
  open! Bits
  module Gf = Gf.Make (Bits)

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

  let rec loop3 ~input ~i ~j ~k ~m ~w ~w_m =
    if j >= m
    then ()
    else (
      (* XXX aray: Remove. For debugging the address controller. *)
      (* Stdio.printf "%i %i %i %i\n" i j k m; *)
      (* Stdio.printf "%i %i\n" (k + j) (j + k + m); *)
      let t = input.(k + j + m) in
      let t = Gf.(t *: w) in
      let tmp = input.(k + j) in
      let tmp = Gf.(tmp -: t) in
      input.(k + j + m) <- tmp;
      input.(k + j) <- Gf.(input.(k + j) +: t);
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
end
