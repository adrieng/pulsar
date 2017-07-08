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

let ( + ) = Int.add
let ( - ) = Int.sub
let ( * ) = Int.mul
let ( / ) = Int.div

type t = { l : Int.t; u : Int.t; }

let make l u =
  assert (l <= u);
  { l = l; u = u; }

let make_0_n u = make Int.zero u

let print fmt { l; u; } =
  Format.fprintf fmt "[%a, %a]" Int.print l Int.print u

let width { l; u; } = Int.abs (Int.succ (u - l))

let singleton i = { l = i; u = i; }

let is_singleton { l; u; } = if l = u then Some l else None

let range { l = l; u = u; } =
  let rec walk i j acc =
    if i > j then acc else walk (Int.succ i) j (i :: acc)
  in
  List.rev (walk l u [])

(** Useful shortcuts *)

let bool = { l = Int.zero; u = Int.one; }

let int = { l = Int.min_int; u = Int.max_int; }

(** Abstract operators *)

let le { l = a; u = b; } { l = c; u = d; } = a >= c && b <= d

let join { l = a; u = b; } { l = c; u = d; } = make (min a c) (max b d)

let meet { l = a; u = b; } { l = c; u = d; } = make (max a c) (min b d)

let neg { l; u; } = make (Int.neg u) (Int.neg l)

let add { l = a; u = b; } { l = c; u = d; } =
  if Utils.add_overflow a c || Utils.add_overflow b d
  then int
  else make (a + c) (b + d)

let sub i1 i2 = add i1 (neg i2)

let mul { l = a; u = b; } { l = c; u = d; } =
  let ac = a * c and ad = a * d and bc = b * c and bd = b * d in
  make (min (min ac ad) (min bc bd)) (max (max ac ad) (max bc bd))

let div { l = a; u = b; } { l = c; u = d; } =
  if c <= Int.zero && Int.zero <= d then int
  else
    let ac = a / c and ad = a / d and bc = b / c and bd = b / d in
    make (min (min ac ad) (min bc bd)) (max (max ac ad) (max bc bd))

let compare = Pervasives.compare
