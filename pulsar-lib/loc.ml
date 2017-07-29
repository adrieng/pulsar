(* This file is part of Pulsar, a temporal functional language.
 * Copyright (C) 2017 Adrien Guatto
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

let (<=) p1 p2 =
  p1.lnum <= p2.lnum || (p1.lnum = p2.lnum && p1.cnum <= p2.cnum)

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

let print_pos fmt { lnum; cnum; } =
  Format.fprintf fmt "%d:%d" lnum cnum

let dummy_pos = { lnum = -1 ; cnum = -1; }

let pos_of_lexing_pos { Lexing.pos_lnum; Lexing.pos_bol; Lexing.pos_cnum; } =
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

let print fmt ({ fn; start; stop; } as loc) =
  if loc <> nowhere
  then
    Format.fprintf fmt "%s %a-%a"
      fn
      print_pos start
      print_pos stop

let print_sameline fmt { fn; start; stop; }  =
  if start.lnum <> stop.lnum then invalid_arg "print_loc_sameline";
  Format.fprintf fmt "\"%s\", line %d, characters %d-%d"
    fn
    start.lnum
    start.cnum
    stop.cnum

let join l1 l2 =
  if l1.fn <> l2.fn then invalid_arg "join";
  {
    fn = l1.fn;
    start = min l1.start l2.start;
    stop = max l1.stop l2.stop;
  }

type 'a located =
  {
    contents : 'a;
    loc : t;
  }

let print_located print_contents ?(disp_loc = false) fmt { contents; loc; } =
  if disp_loc
  then Format.fprintf fmt "@[%a:@ %a@]" print loc print_contents contents
  else print_contents fmt contents
