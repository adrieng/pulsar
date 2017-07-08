(* Copyright (C) Adrien Guatto <adrien.guatto@laposte.net> 2012
 *
 * This file is part of nsched.
 *
 * nsched is free software: you can redistribute it and/or modify it under the
 * terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any later
 * version.
 *
 * nsched is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
 * A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * nsched. If not, see <http://www.gnu.org/licenses/>.
 *)

type t = private { l : Int.t; u : Int.t }

val make : Int.t -> Int.t -> t

val make_0_n : Int.t -> t

val print : Format.formatter -> t -> unit

val width : t -> Int.t

val singleton : Int.t -> t

val is_singleton : t -> Int.t option

val range : t -> Int.t list

val bool : t

val int : t

(** [le x y] is true iff the concretization of x is a subset of the
    concretization of y *)
val le : t -> t -> bool

val meet : t -> t -> t

val join : t -> t -> t

val add : t -> t -> t

val sub : t -> t -> t

val mul : t -> t -> t

val div : t -> t -> t

val compare : t -> t -> int
