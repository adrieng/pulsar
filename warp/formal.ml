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
  | Warp of Periodic.t
  | On of t * t

let rec print fmt ck =
  match ck with
  | Warp p ->
    Periodic.print fmt p
  | On (ck1, ck2) ->
    Format.fprintf fmt "@[%a@ %a %a@]"
      print ck1
      Print.pp_circledast ()
      print ck2

let print fmt ck =
  match ck with
  | On _ ->
     Format.fprintf fmt "(@[%a@])"
       print ck
  | _ ->
     print fmt ck

let rec normalize ck =
  match ck with
  | Warp p ->
    p
  | On (ck1, ck2) ->
    Periodic.on (normalize ck1) (normalize ck2)

let compare ck1 ck2 =
  Periodic.compare (normalize ck1) (normalize ck2)

let equal ck1 ck2 =
  Periodic.equal (normalize ck1) (normalize ck2)

let periodic p =
  Warp p

let zero =
  periodic Periodic.zero

let one =
  periodic Periodic.one

let omega =
  periodic Periodic.omega

let zero_one =
  let prefix = Word.singleton 0 in
  let ppattern = Word.singleton 1 in
  periodic (Periodic.pattern ~prefix ~ppattern)

let on ck1 ck2 =
  let res = On (ck1, ck2) in
  if equal ck1 one || equal ck1 omega || equal ck2 one || equal ck2 omega
  then Warp (normalize res)
  else res

let ( <= ) p q =
  Periodic.(normalize p <= normalize q)

let div p q =
  Warp (Periodic.div (normalize p) (normalize q))

let size p q =
  Periodic.size (normalize p) (normalize q)
