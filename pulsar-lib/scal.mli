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

(** Scalar values *)
type t =
  | Bool of bool
  | Char of char
  | Int of int
  | Float of float

(** Pretty-print a scalar value *)
val print : Format.formatter -> t -> unit

(** Comparison function for scalars a la [Pervasive.compare] *)
val compare : t -> t -> int

(** Compute the type of a scalar value, which is always a base type *)
val type_of : t -> Type.base
