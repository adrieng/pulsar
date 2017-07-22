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

type t =
  | Inf
  | Fin of int

include Utils.PrintableOrderedType with type t := t

val equal : t -> t -> bool

val zero : t

val one : t

val succ : t -> t

val ( <= ) : t -> t -> bool

val ( + ) : t -> t -> t

val ( - ) : t -> t -> t

val ( * ) : t -> t -> t

val ( / ) : t -> t -> t

val min : t -> t -> t

val max : t -> t -> t

val of_int : int -> t

exception Too_big

val to_int : t -> int

val print_utf8 : bool ref
