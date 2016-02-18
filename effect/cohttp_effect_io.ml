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

let open_in str =
  {
    str = str;
    pos = 0;
    len = String.length str;
  }

module M = struct

  type conn = buf
  type ic = buf

  (* output channels are just buffers *)
  type oc = Buffer.t

  effect Readline : ic -> string option
  effect Read : ic * int -> string (* CR: severe type inference error... *)
  effect Write: oc * string -> unit
  effect Flush: oc -> unit

  (* the following read/write logic has only been lightly tested... *)
  let read_rest x =
    let s = String.sub x.str x.pos (x.len-x.pos) in
    x.pos <- x.len;
    s

  let read_line' x =
    if x.pos < x.len then
      let start = x.pos in
      try
        while x.str.[x.pos] != '\n' do
          x.pos <- x.pos + 1
        done;
        let l = if x.pos > 0 && x.str.[x.pos-1] = '\r' then x.pos-start-1 else x.pos-start in
        let s = String.sub x.str start l in
        x.pos <- x.pos + 1;
        Some s
      with _ ->
        Some (read_rest x)
    else
      None

  let read_line x = read_line' x

  let read_exactly' x n =
    if x.len-x.pos < n then None
    else begin
      let s = String.sub x.str x.pos n in
      x.pos <- x.pos + n;
      Some s
    end

  let read x n =
    match read_exactly' x n with
    | None when x.pos >= x.len -> raise End_of_file
    | None -> read_rest x
    | Some x -> x

  let write x s = Buffer.add_string x s

  let flush x = ()

  let run f =
    match f () with
    | () -> ()
    | exception End_of_file -> raise End_of_file
    | effect (Readline ic) k ->
      let s = read_line ic in
      continue k s
    | effect (Read (ic, n)) k ->
      let s = read n ic in
      continue k s
    | effect (Write (oc, s)) k ->
      continue k (write s oc)
    | effect (Flush oc) k ->
      continue k (flush oc)

  let read_line ic = perform (Readline ic)
  let read ic n = perform (Read (n, ic))
  let write oc s = perform (Write (s, oc))
  let flush oc = perform (Flush oc)

end

include M

module Test = struct

  let operations () =
    let ic, oc = open_in "abcdefg", Buffer.create 64 in
    print_endline (read ic 2);
    print_endline (read ic 2);
    write oc "123";
    print_endline (read ic 2);
    write oc "456";
    print_endline (read ic 2);
    print_endline (Buffer.contents oc)

  let test () = run operations
end
