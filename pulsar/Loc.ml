(* This file is part of Pulsar, a temporal functional language.
 * Copyright (C) 2017-2020 Adrien Guatto
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the LICENSE file in the top-level directory.
 *)

type filename = string

type pos =
  {
    lnum : int;
    cnum : int;
  }

let ( <= ) p1 p2 =
  p1.lnum < p2.lnum || (p1.lnum = p2.lnum && p1.cnum <= p2.cnum)

let min p1 p2 =
  if p1 <= p2 then p1 else p2

let max p1 p2 =
  if p1 <= p2 then p2 else p1

let make_pos ~lnum ~cnum =
  if lnum < 0 then invalid_arg "make_pos: negative line number"
  else if cnum < 0 then invalid_arg "make_pos: negative column number"
  else { lnum; cnum; }

let lnum { lnum; _ } = lnum

let cnum { cnum; _ } = cnum

let print_pos { lnum; cnum; } =
  let open PPrint in
  Wdr.ExtPPrint.int lnum ^^ colon ^^ Wdr.ExtPPrint.int cnum

let dummy_pos = { lnum = -1 ; cnum = -1; }

let pos_of_lexing_pos { Lexing.pos_lnum; Lexing.pos_bol; Lexing.pos_cnum; _ } =
  { lnum = pos_lnum + 1; cnum = pos_cnum - pos_bol; }

type t =
  {
    fn : filename;
    start : pos;
    stop : pos;
  }

let make ~fn ~start ~stop =
  if fn = "" then invalid_arg "make_loc: empty file name"
  else if not (start <= stop) then invalid_arg "make_loc: stop < start"
  else { fn; start; stop; }

let file { fn; _ } =
  fn

let range { start; stop; _ } =
  start, stop

let of_lexing_pos_pair ~start ~stop =
  let fn = start.Lexing.pos_fname in
  if fn <> stop.Lexing.pos_fname
  then invalid_arg "loc_of_lexing_pos_pair";
  let start = pos_of_lexing_pos start in
  let stop = pos_of_lexing_pos stop in
  make ~fn ~start ~stop

let nowhere =
  { fn = ""; start = dummy_pos; stop = dummy_pos; }

let print ({ fn; start; stop; } as loc) =
  let open PPrint in
  if loc = nowhere
  then empty
  else string fn ^^ space ^^ print_pos start ^^ caret ^^ print_pos stop

let print_sameline { fn; start; stop; }  =
  let open PPrint in
  let open Wdr.ExtPPrint in
  if start.lnum <> stop.lnum then invalid_arg "print_loc_sameline";
  dquotes (!^ fn) ^^ !^ ", line " ^^ int start.lnum
  ^^ !^ ", characters " ^^ int start.cnum ^^ caret ^^ int stop.cnum

let join l1 l2 =
  if l1 = nowhere then l2
  else if l2 = nowhere then l1
  else if l1.fn <> l2.fn then invalid_arg "join"
  else
    {
      fn = l1.fn;
      start = min l1.start l2.start;
      stop = max l1.stop l2.stop;
    }

let is_in loc pos =
  loc.start <= pos && pos <= loc.stop

let print_located ?(disp_loc = false) doc loc =
  let open PPrint in
  if disp_loc
  then Wdr.ExtPPrint.gparens (doc ^^ !^ ":" ^/^ print loc)
  else doc
