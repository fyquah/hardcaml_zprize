(** Module to reduce numbers modulo a fixed constant with just two subtraction stages.

    {%html: <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/katex@0.16.2/dist/katex.min.css" integrity="sha384-bYdxxUwYipFNohQlHt0bjN/LCpueqWz13HufFEV1SUatKs1cm4L6fFgCi1jT643X" crossorigin="anonymous">
    <script defer src="https://cdn.jsdelivr.net/npm/katex@0.16.2/dist/katex.min.js" integrity="sha384-Qsn9KnoKISj6dI8g7p1HBlNpVx0I8p1SvlwOldgi3IorMle61nQy4zEahWYtljaz" crossorigin="anonymous"></script>
    <script defer src="https://cdn.jsdelivr.net/npm/katex@0.16.2/dist/contrib/auto-render.min.js" integrity="sha384-+VBxd3r6XgURycqtZ117nYw44OOcIax56Z4dCRWbxyPt0Koah1uHoK0o4+/RRE05" crossorigin="anonymous"
        onload="renderMathInElement(document.body);"></script> %}


    Given a fixed modular base \(p\) with \(n\) bits and a multiplicative error \(E\) with \(e\) bits, 
    this module can reduce any input \(N\in\[0, Ep)\) to an equivalent value modulo \(p\) in the range
    \(\[0, p)\) with just two subtraction stages.

    The module accomplishes this by precomputing and loading a ROM with \(2^e\) entries. We set \(R\[0\] = 0\). Then,
    for \(i\geq1\), entry \(R\[i\]\) holds the final \(n\) bits of the largest multiple of \(p\) that has \(i-1\) as its \(e\)-bit
    prefix when written with \(e+n\) bits. In particular,

    {%html: $$R[i] = \left\lfloor \frac{(i-1)\cdot 2^n}{p} \right\rfloor\pmod {2^{n}} $$ %}

    Then, given an input \(N\in\[0,Ep)\) with \(e+n\) bits, we can lookup its \(e\)-bit prefix in the ROM
    and subtract the resulting value from the \(n\)-bit suffix of our input value:

    {%html: $$ N[n-1:0] - R\left[N[e-1+n:n]\right] $$ %}

    (note that we only need to use the lower \(n\) bits because we know what the higher bits are by construction). Then,
    when the lookup index is nonzero, we have to invert the MSB of the signed result to get an unsigned reduction 
    \(n^\prime\) to the range \(\[0,3p)\).

    After this, we do one more subtraction stage where we just multiplex between \(\\{n^\prime, n^\prime-p, n^\prime-2p\\}\)
    to choose the final result.

    This module can be generalized over any modular base \(p\) (not necessarily prime), rather than just our
    given prime. However, this requires a bit of care to double check the bounds above - in our case, the first
    ROM reduction goes to the range \(\[0,3p)\), but for some bases, it is possible that the reduction only
    goes to \(\[0,4p)\) and requires one more subtractor in the subsequent stage.
*)

open Hardcaml

module type Config = sig
  val p : Z.t
  val adder_stages : int
  val num_bits : int
  val error_bits : int
end

module Make (Config : Config) : sig
  val latency : int

  module I : sig
    type 'a t =
      { clock : 'a
      ; coarse_value : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O : sig
    type 'a t = { reduced_value : 'a } [@@deriving sexp_of, hardcaml]
  end

  val create : build_mode:Build_mode.t -> Scope.t -> Signal.t I.t -> Signal.t O.t

  val hierarchical
    :  ?build_mode:Build_mode.t
    -> ?instance:string
    -> Scope.t
    -> Circuit.With_interface(I)(O).create
end
