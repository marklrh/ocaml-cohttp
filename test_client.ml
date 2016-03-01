(* ocamlbuild -use-ocamlfind -pkg cohttp,cohttp.effect test_client.native *)

open Cohttp
open Cohttp_effect

let parse () =
  let open Cohttp_effect in
  let file =
    Uri.of_string "~/test_file.txt" in
  Client.get file

let print b =
  let open Cohttp_effect_body in
  print_endline (to_string b)

let () = parse () |> print body

