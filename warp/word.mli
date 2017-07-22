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

type elem = int

type t

val empty : t

val singleton : elem -> t

val concat : t list -> t

val (^^) : t -> t -> t

val power : t -> int -> t

val of_list : int list -> t

val split_at : int -> t -> t * t

val drop : int -> t -> t

val rotate : t -> t

val rev : t -> t

val at : t -> int -> elem

val length : t -> int

val weight : t -> elem

val is_empty : t -> bool

val has_null_weight : t -> bool

val all_equal : elem -> t -> bool

include Utils.PrintableOrderedType with type t := t

val equal : t -> t -> bool

val to_seq : t -> elem Seq.t

val find_first_non_null_index : t -> int
