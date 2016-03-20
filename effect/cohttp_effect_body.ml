(*{{{ Copyright (c) 2016 Runhang Li <obj@posteo.de>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
  }}}*)

open Cohttp

module IO = Cohttp_effect_io

module EStream = struct
  type t = {
    fin : bool ref;
    stream   : unit -> string option;
  }

  let iter_s fn t =
    let rec loop () =
      match t.stream () with
      | Some c -> fn c; loop ()
      | None -> () in
    loop ()

  let map f t =
    let stream = fun () ->
      match t.stream () with
      | Some r -> Some (f r)
      | None -> None in
    {fin = t.fin; stream}

  let rec junk t =
    match t.stream () with
    | None -> ()
    | Some c -> ignore c; junk t

end

type estream = EStream.t

type t = [
  | Body.t
  | `Stream of estream
]

let empty = (Body.empty :> t)

let create_stream fn arg =
  let open EStream in
  let fin = ref false in
  let stream = fun () ->
    match !fin with
    | true -> None
    | false -> begin
      match fn arg with
      | Transfer.Done -> fin := true; None
      | Transfer.Final_chunk c -> fin := true; Some c
      | Transfer.Chunk c -> Some c
      end in
  {fin; stream}

let is_empty (body:t) =
  match body with
  | #Body.t as body -> Body.is_empty body
  | `Stream s -> !(s.fin)

let to_string (body:t) =
  match body with
  | #Body.t as body -> Body.to_string body
  |`Stream s -> begin
    let s = (s : estream) in
    let b = Buffer.create 1024 in
    let rec loop () =
      match s.stream () with
      | Some s -> Buffer.add_string b s; loop ()
      | None -> () in
    loop ();
    Buffer.contents b
    end

let to_string (body:t) =
  IO.run (fun () -> to_string body)

let to_string_list (body:t) =
  match body with
  | #Body.t as body -> Body.to_string_list body
  |`Stream s ->
    let rec loop acc =
      match s.stream () with
      | None -> List.rev acc
      | Some s -> loop (s :: acc) in
    loop []

let of_string s = ((Body.of_string s) :> t)

let drain_body (body:t) =
  match body with
  |`Empty |`String _ |`Strings _ -> ()
  |`Stream s -> EStream.junk s

let of_string_list l = `Strings l

let of_stream s = `Stream s

let transfer_encoding = function
  |#Body.t as t -> Body.transfer_encoding t
  |`Stream _ -> Transfer.Chunked

(* This will consume the body and return a length, and a
 * new body that should be used instead of the input *)
let length (body:t) : (int64 * t) =
  match body with
  |#Body.t as body -> Body.length body, body
  |`Stream s ->
    let buf = to_string body in
    let len = Int64.of_int (String.length buf) in
    len, `String buf

let write_body fn = function
  |`Empty -> ()
  |`Stream st -> EStream.iter_s fn st
  |`String s -> fn s
  |`Strings sl -> List.iter fn sl

let map f t =
  match t with
  | #Body.t as t -> (Body.map f t :> t)
  | `Stream s -> `Stream (EStream.map f s)


