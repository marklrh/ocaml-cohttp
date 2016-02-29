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

module Body = Cohttp_effect_body (* not stream *)
module type IO = S.Effect_IO

module S = Cohttp_effect_s

module type Client = S.Client
module type Server = S.Server
module type Net = S.Net

module EMake_request(IO: IO) = struct
  include Cohttp.Request
  include (EMake(IO) : module type of EMake(IO) with type t :=t)
end

module EMake_response(IO: IO) = struct
  include Cohttp.Response
  include (EMake(IO) : module type of EMake(IO) with type t :=t)
end

module Request = Cohttp.Request
module Response = Cohttp.Response

module Make_client
    (IO: IO)
    (Net:Net with module IO = IO) = struct

  module IO = IO
  module Response = EMake_response(IO)
  module Request = EMake_request(IO)

  type ctx = Net.ctx
  let default_ctx = Net.default_ctx

  let read_response ~closefn ic oc meth =
    match Response.read ic with
    | `Invalid reason ->
      failwith ("Failed to read response: " ^ reason)
    | `Eof -> failwith "Client connection was closed"
    | `Ok res -> begin
        let has_body = match meth with
          | `HEAD -> `No
          | _ -> Response.has_body res
        in
        match has_body with
        | `Yes | `Unknown ->
          let reader = Response.make_body_reader res ic in
          let stream = Body.create_stream Response.read_body_chunk reader in
          let body = Body.of_stream stream in
          res, body
        | `No ->
          closefn ();
          res, `Empty
      end

  let is_meth_chunked = function
    | `HEAD -> false
    | `GET -> false
    | `DELETE -> false
    | _ -> true

  let call ?(ctx=default_ctx) ?headers ?(body=`Empty) ?chunked meth uri =
    let headers = match headers with None -> Header.init () | Some h -> h in
    let conn, ic, oc = Net.connect_uri ~ctx uri in
    let closefn () = Net.close ic oc in
    let chunked =
      match chunked with
      | None -> is_meth_chunked meth
      | Some v -> v in
    let () = match chunked with
      | true ->
        let req = Request.make_for_client ~headers ~chunked meth uri in
        Request.write (fun writer ->
          Body.write_body (Request.write_body writer) body) req oc
      | false ->
        (* If chunked is not allowed, then obtain the body length and
           insert header *)
        let body_length, buf = Body.length body in
        let req =
          Request.make_for_client ~headers ~chunked ~body_length meth uri
        in
        Request.write (fun writer ->
          Body.write_body (Request.write_body writer) buf) req oc
    in
    read_response ~closefn ic oc meth

  (* The HEAD should not have a response body *)
  let head ?ctx ?headers uri =
    fst (call ?headers `HEAD uri)

  let get ?ctx ?headers uri = call ?ctx ?headers `GET uri
  let delete ?ctx ?body ?chunked ?headers uri =
    call ?ctx ?headers ?body ?chunked `DELETE uri
  let post ?ctx ?body ?chunked ?headers uri =
    call ?ctx ?headers ?body ?chunked `POST uri
  let put ?ctx ?body ?chunked ?headers uri =
    call ?ctx ?headers ?body ?chunked `PUT uri
  let patch ?ctx ?body ?chunked ?headers uri =
    call ?ctx ?headers ?body ?chunked `PATCH uri

  let post_form ?ctx ?headers ~params uri =
    let headers = Header.add_opt_unless_exists headers
                    "content-type" "application/x-www-form-urlencoded" in
    let body = Body.of_string (Uri.encoded_of_query params) in
    post ?ctx ~chunked:false ~headers ~body uri

end

