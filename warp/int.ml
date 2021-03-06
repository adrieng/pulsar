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

include Nativeint

let ( + ) = add

let ( - ) = sub

let ( * ) = mul

let ( / ) = div

let ( mod ) = rem

let mod_b1 x y = succ (pred x mod y)

let div_b1 x y = pred x / y

let div_upper x y = succ (pred x / y)

let ( <= ) = ( <= )

let ( < ) = ( < )

let ( >= ) = ( >= )

let ( > ) = ( > )

let ( = ) i1 i2 = Nativeint.compare i1 i2 = 0

let min = Stdlib.min

let max = Stdlib.max

let of_bool b = if b then one else zero

let of_char c = of_string (String.make 1 c)

let print fmt i = Format.fprintf fmt "%nd" i

let equal = (=)

let compare = Stdlib.compare

let hash = Hashtbl.hash

module Ordered =
struct
  type t = Nativeint.t
  let compare = Nativeint.compare
end

module Env = Map.Make(Ordered)
module Set = Set.Make(Ordered)

let rec gcd a b =
  if b = zero then a else gcd b (a mod b)

let lcm a b =
  let g = gcd a b in
  let g = if g <> zero then g else one in
  (a * b) / g

let rec iter f i acc =
  if i = zero
  then acc
  else iter f (pred i) (f acc)
