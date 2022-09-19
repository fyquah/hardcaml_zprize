open! Core
module Gf = Hardcaml_ntt.Gf.Z
module Ntt = Hardcaml_ntt.Reference_model.Make (Gf)

let command =
  Command.basic
    ~summary:""
    [%map_open.Command
      let infile = anon ("INFILE" %: string)
      and reffile = anon ("REFFILE" %: string) in
      fun () ->
        let load_gf filename =
          In_channel.read_lines filename
          |> List.map ~f:(fun s -> Gf.of_z (Z.of_string s))
          |> Array.of_list
        in
        let inputs = load_gf infile in
        let refs = load_gf reffile in
        if Array.length inputs <> Array.length refs
        then raise_s [%message "input and reference files are not the same length"];
        Ntt.inverse_dit inputs;
        if [%compare.equal: Gf.t array] inputs refs
        then raise_s [%message "Test failed." (inputs : Gf.t array)]]
;;

let () = Command_unix.run command
