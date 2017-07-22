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
    {
      num : Int.t;
      den : Int.t;
    }

let print fmt { num; den; } =
  Format.fprintf fmt "@[%a / %a@]"
    Int.print num
    Int.print den

let make num den =
  let open Int in
  assert (not (den = zero));
  let k =
    let k = gcd num den in
    if k <> zero then k else one
  in
  { num = num / k; den = den / k; }

let int num =
  { num; den = Int.one; }

let to_float { num; den; } =
  Int.to_float num /. Int.to_float den

let ( = ) r1 r2 =
  let open Int in
  r1.num * r2.den = r2.num * r1.den

let ( <= ) r1 r2 =
  let open Int in
  r1.num * r2.den <= r2.num * r1.den

let ( < ) r1 r2 =
  let open Int in
  r1.num * r2.den < r2.num * r1.den

let ( >= ) r1 r2 =
  r2 <= r1

let ( > ) r1 r2 =
  r2 < r1

let neg { num; den; } =
  { num = Int.neg num; den = den; }

let inv { num; den; } =
  if Int.(den = zero) then invalid_arg "inv: zero";
  make den num

let ( + ) r1 r2 =
  let open Int in
  make (r1.num * r2.den + r2.num * r1.den) (r1.den * r2.den)

let ( - ) r1 r2 =
  r1 + neg r2

let ( * ) r1 r2 =
  let open Int in
  make (r1.num * r2.num) (r1.den * r2.den)

let ( / ) r1 r2 =
  r1 * inv r2
