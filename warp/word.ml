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

type t =
  {
    desc : desc;
    length : int;
    weight : elem;
  }

and desc =
  | Single of elem
  | Concat of t list
  | Power of t * int

let rec print_short fmt w =
  let open Print in
  match w.desc with
  | Single i ->
     Format.fprintf fmt "%d" i
  | Concat w_l ->
     pp_list print_short fmt w_l
  | Power ({ desc = Single i; _ }, j) ->
     Format.fprintf fmt "%d^%d"
       i
       j
  | Power (w, j) ->
     Format.fprintf fmt "{ %a }^%d"
       print_short w
       j

let rec print_full fmt w =
  let open Print in
  match w.desc with
  | Single i ->
     Format.fprintf fmt "%d" i
  | Concat w_l ->
     Format.fprintf fmt "{ @[%a@] }"
       (pp_list print_full) w_l
  | Power (w, j) ->
     Format.fprintf fmt "%a^%d"
       print_full w
       j

let print =
  print_short

let empty =
  {
    desc = Concat [];
    length = 0;
    weight = 0;
  }

let singleton m =
  {
    desc = Single m;
    length = 1;
    weight = m;
  }

let rec concat w_l =
  let rec simplify racc w_l =
    match w_l with
    | [] ->
       List.rev racc
    | w :: w_l ->
       if w.length = 0
       then simplify racc w_l
       else simplify (w :: racc) w_l
  in
  let w_l = simplify [] w_l in
  match w_l with
  | [] ->
     empty
  | [w] ->
     w
  | _ :: _ ->
     {
       desc = Concat w_l;
       length = List.fold_left (fun n t -> n + t.length) 0 w_l;
       weight = List.fold_left (fun m t -> m + t.weight) 0 w_l;
     }

let (^^) w1 w2 =
  concat [w1; w2]

let power w n =
  if n = 0 then empty
  else if n = 1 then w
  else
    {
      desc = Power (w, n);
      length = w.length * n;
      weight = w.weight * n;
    }

let of_list l =
  List.fold_left (fun w i -> w ^^ singleton i) empty l

let rec split_at n w =
  assert (n >= 0 && n <= w.length);
  if n = 0 then (empty, w)
  else if n > w.length then invalid_arg "split_at: word too short"
  else match w.desc with
       | Single _ ->
          if n = 0 then empty, w else w, empty
       | Concat w_l ->
          let rec find rpref n w_l =
            match w_l with
            | [] ->
               rpref, w_l
            | w :: w_l ->
               if n < w.length
               then
                 let w', w'' = split_at n w in
                 (w' :: rpref, w'' :: w_l)
               else find (w :: rpref) (n - w.length) w_l
          in
          let rpref, rest = find [] n w_l in
          concat (List.rev rpref), concat rest
       | Power (w, i) ->
          let n' = n mod w.length in
          let i' = n / w.length in
          let w', w'' = split_at n' w in
          concat [power w i'; w'], concat [w''; power w (i - i' - 1)]

let rec drop n w =
  snd (split_at n w)

let rotate w =
  let w1, w2 = split_at 1 w in
  w2 ^^ w1

let rec rev w =
  let wd =
    match w.desc with
    | Single _ ->
       w.desc
    | Concat w_l ->
       Concat (List.rev_map rev w_l)
    | Power (w, n) ->
       Power (rev w, n)
  in
  { w with desc = wd; }

let rec at w i =
  assert (i >= 0 && i < w.length);
  match w.desc with
  | Single m ->
     m
  | Concat w_l ->
     at_l w_l i
  | Power (w, j) ->
     at w (i mod w.length * j)

and at_l w_l i =
  (* assert (i >= 0 && i < List.length w_l); *)
  match w_l with
  | [] ->
     assert false
  | w :: w_l ->
     if i < w.length then at w i else at_l w_l (i - w.length)

let length w =
  w.length

let weight w =
  w.weight

let is_empty w =
  w.length = 0

let has_null_weight w =
  w.weight = 0

let rec all_equal m w =
  w.length = 0 ||
    match w.desc with
    | Single m' ->
       m = m'
    | Concat w_l ->
       List.for_all (all_equal m) w_l
    | Power (w, _) ->
       all_equal m w

let compare w1 w2 =
  if w1 == w2 then 0
  else
    let rec compare_desc i =
      assert (i >= 0 && i <= w1.length);
      if i = w1.length then 0
      else
        Utils.compare_both
          (Utils.compare_int (at w1 i) (at w2 i))
          (fun () -> compare_desc (i + 1))
    in
    Utils.(compare_both
             (compare_int w1.length w2.length)
             (fun () ->
               compare_both
                 (compare_int w1.weight w2.weight)
                 (fun () -> compare_desc 0)))

let equal w1 w2 =
  compare w1 w2 = 0

let to_seq w k =
  let rec iter w =
    match w.desc with
    | Single n ->
       k n
    | Concat w_l ->
       List.iter iter w_l
    | Power (w, n) ->
       for i = 1 to n do
         iter w
       done
  in
  iter w

exception Found

let find_first_non_null_index w =
  let r = ref 0 in
  if w.weight = 0 then invalid_arg "find_first_non_null_index";
  try
    to_seq w (fun i -> if i > 0 then raise Found);
    assert false;
  with Found -> !r
