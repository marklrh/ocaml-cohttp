open Cohttp

(** Portable Lwt implementation of HTTP client and server, without
    depending on a particular I/O implementation.  The various [Make]
    functors must be instantiated by an implementation that provides
    a concrete IO monad. *)

module type IO = S.Effect_IO
(** The IO module is specialized for the [Lwt] monad. *)

(** The [Net] module type defines how to connect to a remote node
    and close the resulting channels to clean up. *)
module type Net = sig
  module IO : IO
  val connect_uri : Uri.t -> (IO.conn * IO.ic * IO.oc) (* CR: may need new effect? *)
  val close_in : IO.ic -> unit
  val close_out : IO.oc -> unit
  val close : IO.ic -> IO.oc -> unit
end

(** The [Client] module implements non-pipelined single HTTP client
    calls.  Each call will open a separate {! Net } connection.  For
    best results, the {! Cohttp_lwt_body } that is returned should be
    consumed in order to close the file descriptor in a timely
    fashion.  It will still be finalized by a GC hook if it is not used
    up, but this can take some additional time to happen. *)
module type Client = sig

  val call :
    ?headers:Cohttp.Header.t ->
    ?body:Cohttp_effect_body.t ->
    ?chunked:bool ->
    Cohttp.Code.meth ->
    Uri.t -> (Response.t * Cohttp_effect_body.t)

  val head :
    ?headers:Cohttp.Header.t ->
    Uri.t -> Response.t

  val get :
    ?headers:Cohttp.Header.t ->
    Uri.t -> (Response.t * Cohttp_effect_body.t)

  val delete :
    ?body:Cohttp_effect_body.t ->
    ?chunked:bool ->
    ?headers:Cohttp.Header.t ->
    Uri.t -> (Response.t * Cohttp_effect_body.t)

  val post :
    ?body:Cohttp_effect_body.t ->
    ?chunked:bool ->
    ?headers:Cohttp.Header.t ->
    Uri.t -> (Response.t * Cohttp_effect_body.t)

  val put :
    ?body:Cohttp_effect_body.t ->
    ?chunked:bool ->
    ?headers:Cohttp.Header.t ->
    Uri.t -> (Response.t * Cohttp_effect_body.t)

  val patch :
    ?body:Cohttp_effect_body.t ->
    ?chunked:bool ->
    ?headers:Cohttp.Header.t ->
    Uri.t -> (Response.t * Cohttp_effect_body.t)

  val post_form :
    ?headers:Cohttp.Header.t ->
    params:(string * string list) list ->
    Uri.t -> (Response.t * Cohttp_effect_body.t)

(*
  val callv :
    ?ctx:ctx ->
    Uri.t ->
    (Request.t * Cohttp_lwt_body.t) Lwt_stream.t ->
    (Response.t * Cohttp_lwt_body.t) Lwt_stream.t Lwt.t
*)
end

(** The [Server] module implements a pipelined HTTP/1.1 server. *)
(*
module type Server = sig
  module IO : IO

  type conn = IO.conn * Cohttp.Connection.t

  type t

  val make : ?conn_closed:(conn -> unit)
    -> callback:(conn -> Cohttp.Request.t -> Body.t
                 -> (Cohttp.Response.t * Body.t))
    -> unit -> t

  (** Resolve a URI and a docroot into a concrete local filename. *)
  val resolve_local_file : docroot:string -> uri:Uri.t -> string

  (** [respond ?headers ?flush ~status ~body] will respond to an HTTP
    request with the given [status] code and response [body].  If
    [flush] is true, then every response chunk will be flushed to
    the network rather than being buffered. [flush] is true by default. 
    The transfer encoding will be detected from the [body] value and
    set to chunked encoding if it cannot be determined immediately.
    You can override the encoding by supplying an appropriate [Content-length]
    or [Transfer-encoding] in the [headers] parameter. *)
  val respond :
    ?headers:Cohttp.Header.t ->
    ?flush:bool ->
    status:Cohttp.Code.status_code ->
    body:Body.t -> unit -> (Response.t * Body.t)

  val respond_string :
    ?headers:Cohttp.Header.t ->
    status:Cohttp.Code.status_code ->
    body:string -> unit -> (Response.t * Body.t)

  val respond_error :
    ?headers:Header.t ->
    ?status:Cohttp.Code.status_code ->
    body:string -> unit -> (Response.t * Body.t)

  val respond_redirect :
    ?headers:Cohttp.Header.t ->
    uri:Uri.t -> unit -> (Response.t * Body.t)

  val respond_need_auth :
    ?headers:Cohttp.Header.t ->
    auth:Cohttp.Auth.challenge -> unit -> (Response.t * Body.t)

  val respond_not_found :
    ?uri:Uri.t -> unit -> (Response.t * Body.t)

  val callback : t -> IO.conn -> IO.ic -> IO.oc -> unit

end
*)
