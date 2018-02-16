(* This file is part of Pulsar, a temporal functional language.
 * Copyright (C) 2017,2018 Adrien Guatto
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

module AnnotKind =
struct
  type t =
    | Typing
    | Subtyping

  let print fmt k =
    match k with
    | Typing ->
       Format.fprintf fmt ":"
    | Subtyping ->
       Format.fprintf fmt "<:"

  let compare k1 k2 =
    let tag_to_int k =
      match k with
      | Typing -> 0
      | Subtyping -> 1
    in
    match k1, k2 with
    | Typing, Typing | Subtyping, Subtyping ->
       0
    | (Typing | Subtyping), _ ->
       Warp.Utils.compare_int (tag_to_int k1) (tag_to_int k2)
end

module BlockKind =
struct
  type t =
    | Seq
    | Par
    | Rec

  let default =
    Seq

  let print fmt k =
    match k with
    | Seq ->
       Format.fprintf fmt "seq"
    | Par ->
       Format.fprintf fmt "par"
    | Rec ->
       Format.fprintf fmt "rec"

  let compare k1 k2 =
    let tag_to_int k =
      match k with
      | Seq -> 0
      | Par -> 1
      | Rec -> 2
    in
    if k1 = k2
    then 0
    else Warp.Utils.compare_int (tag_to_int k1) (tag_to_int k2)
end
