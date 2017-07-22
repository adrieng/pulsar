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

(** {2 Compiler configuration} *)

type ctx

val make_default : filename:string -> ctx

val current_file : ctx -> string

(** {2 Passes} *)

type 'a t

val atomic :
  ?pp_in : 'a Warp.Print.printer ->
  ?pp_out : 'b Warp.Print.printer ->
  name:string ->
  (ctx -> 'a -> 'b) ->
  ('a -> 'b) t

val ( >>> ) : ('a -> 'b) t -> ('b -> 'c) t -> ('a -> 'c) t

val run : ctx:ctx -> ('a -> 'b) t -> 'a -> 'b

val command_line_arguments : 'a t -> (string * Arg.spec * string) list
