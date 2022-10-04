(** Module to transform scalars to a signed-digit representation, which is expected
    by the downstream FPGA logic.

    {%html: <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/katex@0.16.2/dist/katex.min.css" integrity="sha384-bYdxxUwYipFNohQlHt0bjN/LCpueqWz13HufFEV1SUatKs1cm4L6fFgCi1jT643X" crossorigin="anonymous">
    <script defer src="https://cdn.jsdelivr.net/npm/katex@0.16.2/dist/katex.min.js" integrity="sha384-Qsn9KnoKISj6dI8g7p1HBlNpVx0I8p1SvlwOldgi3IorMle61nQy4zEahWYtljaz" crossorigin="anonymous"></script>
    <script defer src="https://cdn.jsdelivr.net/npm/katex@0.16.2/dist/contrib/auto-render.min.js" integrity="sha384-+VBxd3r6XgURycqtZ117nYw44OOcIax56Z4dCRWbxyPt0Koah1uHoK0o4+/RRE05" crossorigin="anonymous"
        onload="renderMathInElement(document.body);"></script> %}

    The main transform currently implemented transforms the input scalar from unsigned digits
    in the range {%html: \([0,2^b-1]\) %} to signed digits in the range {%html: \([-2^{b-1}, 2^{b-1}-1]\) %}.

    It does this as follows: suppose we have a B-bit scalar {%html: \(k\)%}, split into {%html: \(N\) %}
    windows of size {%html: \(b_i\) %}, where {%html: \(\sum_{i=0}^{N-1}{b_i} = B\) %}. Then, letting
    {%html: \(o_i = \sum_{j=0}^{i-1}{b_i}\) %} be the offsets of each digit, we can write
    {%html: $$ k = \sum_{i=0}^{N-1}{2^{o_i}d_i},\;\; d_i\in[0,2^b_i-1]\;\forall i  $$ %}
    as a normal unsigned representation of {%html: \(k\) %} (derived by just directly windowing the bits in its
    binary representation).

    Then, we perform the following iterative transform from {%html: \(i = 0\) %} to {%html: \(N-2\) %}.

    {%html: 
    $$ 
    \text{If } d_i \geq 2^{b_i-1}:
    (d_i, d_{i+1}) \leftarrow (d_i - 2^{b_i}, d_{i+1} + 1)
    $$
    %}

    After performing this transform, digits {%html: \(d_i\in[-2^{b_i-1}, 2^{b_i-1}-1]\) %} for {%html: \(i\in[0,N-2]\) %}.

    The downstream point adder can exploit this new digit base because point negation is extremely cheap (on the Twisted Edwards
    affine space, it just corresponds to negative the x-coordinate). So, for all but the final window,
    we can halve the number of buckets and use point subtraction for all the negative buckets.

    The module implements this transform as a fully unrolled (N-1)-stage pipeline, with optional skid buffers
    in order to cut combinational ready signal paths. It is designed in a modular way so that the 
    overall scalar transformation can be extended to include many further transforms using other 
    scalars (i.e. 2, 3, etc.), if point multiples were precomputed and loaded in the FPGA.
*)

open Hardcaml
module Axi512 = Hardcaml_axi.Axi512

module Make (Config : Config.S) : sig
  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; host_scalars_to_fpga : 'a Axi512.Stream.Source.t
      ; transformed_scalars_to_fpga_dest : 'a Axi512.Stream.Dest.t
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O : sig
    type 'a t =
      { transformed_scalars_to_fpga : 'a Axi512.Stream.Source.t
      ; host_scalars_to_fpga_dest : 'a Axi512.Stream.Dest.t
      }
    [@@deriving sexp_of, hardcaml]
  end

  val num_scalars_per_ddr_word : int
  val num_scalar_64b_words : int

  val unpack_to_windows_and_negatives
    :  (module Hardcaml.Comb.S with type t = 'a)
    -> 'a
    -> 'a array * 'a array

  val create : Scope.t -> Signal.t I.t -> Signal.t O.t
  val hierarchical : ?instance:string -> Scope.t -> Signal.t Interface.Create_fn(I)(O).t
end
