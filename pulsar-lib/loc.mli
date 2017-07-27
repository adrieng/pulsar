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

(** A (column, line) position in a source file *)
type pos

(** Create a position; [lnum] and [cnum] should be non-negative *)
val make_pos : lnum:int -> cnum:int -> pos

(** Pretty-print a position *)
val print_pos : Format.formatter -> pos -> unit

(** Lexicographic ordering on positions *)
val (<=) : pos -> pos -> bool

(** Return the minimum position in the lexicographic order *)
val min : pos -> pos -> pos

(** Return the maximum position in the lexicographic order *)
val max : pos -> pos -> pos

(** Extract the line number of a position *)
val lnum : pos -> int

(** Extract the column number of a position *)
val cnum : pos -> int

(** Convert from Lexing.pos to our positions *)
val pos_of_lexing_pos : Lexing.position -> pos

(** A source location, that is a range between two position *)
type t

(** Create a location in a source file; raises Invalid_arg if [fn] is empty *)
val make : fn:string -> start:pos -> stop:pos -> t

(** Create a location from a pair of Lexing.position; raises Invalid_arg if the
    positions are not in the same file *)
val of_lexing_pos_pair :
  start:Lexing.position -> stop:Lexing.position -> t

(** Pretty-print a location *)
val print : Format.formatter -> t -> unit

(** The fictional location *)
val nowhere : t

(** computes the smallest location [l] s.t. [l1] and [l2] are both included in
    [l]; raises Invalid_arg if [l1] and [l2] belong to distinct files *)
val join : t -> t -> t

(** A thing together with its location *)
type 'a located =
  {
    contents : 'a;
    loc : t;
  }

(** Pretty-print a thing using the provided pretty-printing function. Display
    its location only if called with [disp_loc] set to true. *)
val print_located :
  (Format.formatter -> 'a -> unit) ->
  ?disp_loc : bool ->
  Format.formatter -> 'a located -> unit
