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

(** The global variable [utf8_output] controls whether UTF-8 characters should
    be emitted by some of the functions below. *)
val utf8_output : bool ref

(** The type of pretty-printers. *)
type 'a printer = Format.formatter -> 'a -> unit

val string_of : 'a printer -> 'a -> string

val pp_nothing : 'a printer

val pp_space : unit printer

val pp_break : unit printer

val pp_breakable_space : unit printer

val pp_strong_break : unit printer

val pp_comma : unit printer

val pp_semicolon : unit printer

val pp_times : unit printer

val pp_arrow : unit printer

val pp_thick_arrow : unit printer

val pp_circledast : unit printer

val pp_lambda : unit printer

val pp_omega : unit printer

val pp_bool : bool printer

val pp_int : int printer

val pp_string : string printer

val pp_pair :
  ?pp_sep:unit printer ->
  'a printer ->
  'b printer ->
  ('a * 'b) printer

val pp_opt :
  ?pp_left:unit printer ->
  ?pp_right:unit printer ->
  'a printer ->
  'a option printer

val pp_list :
  ?pp_left:unit printer ->
  ?pp_right:unit printer ->
  ?pp_sep:unit printer ->
  'a printer ->
  'a list printer

val pp_array :
  ?pp_left:unit printer ->
  ?pp_right:unit printer ->
  ?pp_sep:unit printer ->
  'a printer ->
  'a array printer

val pp_thunk :
  unit printer printer
