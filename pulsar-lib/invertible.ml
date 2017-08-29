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
  | Id
  | Wrap
  | Unwrap
  | Concat of Warp.Formal.t * Warp.Formal.t
  | Decat of Warp.Formal.t * Warp.Formal.t
  | Dist
  | Fact
  | Infl
  | Defl

let print fmt i =
  match i with
  | Id ->
     Format.fprintf fmt "id"
  | Wrap ->
     Format.fprintf fmt "wrap"
  | Unwrap ->
     Format.fprintf fmt "unwrap"
  | Concat (p, q) ->
     Format.fprintf fmt "@[concat@ %a@ %a@]"
       Warp.Formal.print p
       Warp.Formal.print q
  | Decat (p, q) ->
     Format.fprintf fmt "@[decat@ %a@ %a@]"
       Warp.Formal.print p
       Warp.Formal.print q
  | Dist ->
     Format.fprintf fmt "dist"
  | Fact ->
     Format.fprintf fmt "fact"
  | Infl ->
     Format.fprintf fmt "infl"
  | Defl ->
     Format.fprintf fmt "defl"

let compare i1 i2 =
  let tag_to_int i =
    match i with
    | Id -> 0
    | Wrap -> 1
    | Unwrap -> 2
    | Concat _ -> 3
    | Decat _ -> 4
    | Dist -> 5
    | Fact -> 6
    | Infl -> 7
    | Defl -> 8
  in
  match i1, i2 with
  | Id, Id
    | Wrap, Wrap
    | Unwrap, Unwrap
    | Dist, Dist
    | Fact, Fact
    | Infl, Infl
    | Defl, Defl
    ->
     0
  | Concat (p1, q1), Concat (p2, q2)
    | Decat (p1, q1), Decat (p2, q2)
    ->
     Warp.Utils.compare_both
       (Warp.Formal.compare p1 p2)
       (fun () -> Warp.Formal.compare q1 q2)
  | (Id | Wrap | Unwrap | Concat _ | Decat _ | Dist | Fact | Infl
     | Defl), _ ->
     Warp.Utils.compare_int (tag_to_int i1) (tag_to_int i2)

let equal i1 i2 =
  0 = compare i1 i2

let invert i =
  match i with
  | Id ->
     Id
  | Wrap ->
     Unwrap
  | Unwrap ->
     Wrap
  | Concat (p, q) ->
     Decat (p, q)
  | Decat (p, q) ->
     Concat (p, q)
  | Dist ->
     Fact
  | Fact ->
     Dist
  | Infl ->
     Defl
  | Defl ->
     Infl
