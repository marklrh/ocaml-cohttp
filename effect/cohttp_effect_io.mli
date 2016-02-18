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


type buf = {
  str : string;
  mutable pos : int;
  len : int;
}

(** [open_in s] will make the string [s] available as a [buf]
   that can be parsed via Cohttp *)
val open_in : string -> buf

(** [ic] represents an input channel *)
type ic = buf

(** [oc] represents an output channel *)
type oc = Buffer.t

(** [conn] represents the underlying network flow *)
type conn

(** [read_line ic] will read a single line terminated
    by CR or CRLF from the input channel [ic].  It returns
    {!None} if EOF or other error condition is reached. *)
val read_line : ic -> string option

(** [read ic len] will block until a maximum of [len] characters
    are read from the input channel [ic].  It returns an
    empty string if EOF or some other error condition occurs
    on the input channel, and can also return fewer than [len]
    characters if input buffering is not sufficient to satisfy the
    request. *)
val read : ic -> int -> string

(** [write oc s] will block until the complete [s] string is
    written to the output channel [oc]. *)
val write : oc -> string -> unit

(** [flush oc] will return when all previously buffered content
    from calling {!write} have been written to the output channel
    [oc]. *)
val flush : oc -> unit

(** run a sequence of I/O operations *)
val run : (unit -> unit) -> unit
