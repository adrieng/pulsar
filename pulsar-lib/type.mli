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

(** Types of stream elements *)
type base =
  | Unit
  | Bool
  | Char
  | Int
  | Float

(** Pretty-print a base type *)
val print_base : Format.formatter -> base -> unit

(** Comparison function for base types a la [Pervasives.compare]. *)
val compare_base : base -> base -> int

(** Equality testing for base types *)
val equal_base : base -> base -> bool

(** General types *)
type t =
  | Base of base
  | Stream of base
  | Prod of t * t
  | Fun of t * t
  | Warped of Warp.Formal.t * t

(** Pretty-print a type *)
val print : Format.formatter -> t -> unit

(** Compute the normal form of a tpe w.r.t. reversible type laws *)
val normalize : t -> t

(** Pretty-print the normal form of a type *)
val print_normalized : Format.formatter -> t -> unit

(** Comparison function for tpes a la [Pervasives.compare] *)
val compare : t -> t -> int

(** Equalit testing for tpes *)
val equal : t -> t -> bool

(** Equivalence testing for tpes w.r.t. reversible tpe laws *)
val equiv : t -> t -> bool

(** Add the warp equivalent of the later modality *)
val later : t -> t

(** Add the warp equivalent of the constant modality *)
val constant : t -> t

(** partial inverse to [Base]; may raise Invalid_argument *)
val get_base : t -> base

(** partial inverse to [Stream]; may raise Invalid_argument *)
val get_stream : t -> base

(** partial inverse to [Prod]; may raise Invalid_argument *)
val get_prod : t -> t * t

(** partial inverse to [Fun]; may raise Invalid_argument *)
val get_fun : t -> t * t

(** partial inverse to [Warped]; may raise Invalid_argument *)
val get_warped : t -> Warp.Formal.t * t
