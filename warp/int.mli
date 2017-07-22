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

type t

val of_int : int -> t

val to_int : t -> int

val to_float : t -> float

val of_int32 : Int32.t -> t

val add : t -> t -> t

val sub : t -> t -> t

val mul : t -> t -> t

val div : t -> t -> t

val ( + ) : t -> t -> t

val ( - ) : t -> t -> t

val ( * ) : t -> t -> t

val ( / ) : t -> t -> t

val ( mod ) : t -> t -> t

val mod_b1 : t -> t -> t

val div_b1 : t -> t -> t

val div_upper : t -> t -> t

val ( <= ) : t -> t -> bool

val ( < ) : t -> t -> bool

val ( >= ) : t -> t -> bool

val ( > ) : t -> t -> bool

val ( = ) : t -> t -> bool

val min : t -> t -> t

(* val max : t -> t -> t *)

val max : t -> t -> t

val zero : t

val one : t

val abs : t -> t

val succ : t -> t

val pred : t -> t

(* val min_int : t *)

(* val max_int : t *)

val neg : t -> t

val logxor : t -> t -> t

val logor : t -> t -> t

val logand : t -> t -> t

val lognot : t -> t

val of_bool : bool -> t

val of_char : char -> t

val of_string : string -> t

val to_string : t -> string

val print : Format.formatter -> t -> unit

val equal : t -> t -> bool

val compare : t -> t -> int

val hash : t -> int

module Ordered : Map.OrderedType with type t = t

module Env : Map.S with type key = t

module Set : Set.S with type elt = t

val gcd : t -> t -> t

val lcm : t -> t -> t

val iter : ('a -> 'a) -> t -> 'a -> 'a

val min_int : t

val max_int : t
