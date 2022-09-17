(** forward.(i) is the [2^i th root of unity].

    That is index [5] is the [2^5 = 32nd] root of unity. Raising it to the power
   [32] will yield [1]. *)
val forward : Gf_z.t array

(** inverse.(i) is the [1 / 2^i th root of unity] *)
val inverse : Gf_z.t array
