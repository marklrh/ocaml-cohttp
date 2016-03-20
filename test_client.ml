(* ocamlbuild -use-ocamlfind -pkg cohttp,cohttp.effect test_client.native *)

open Cohttp
open Cohttp_effect

let parse () =
  let open Cohttp_effect in
  let file =
    Uri.of_string "./test1.txt" in
  let r, b = Client.get file in
  Printf.printf "%Ld\n" (fst (Cohttp_effect_body.length b))

let () = parse ()
