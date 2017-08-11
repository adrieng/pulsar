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

module Invertible =
struct
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
end

type t =
  | Seq of t * t
  | Arr of t * t
  | Prod of t * t
  | Warped of Warp.Formal.t * t
  | Invertible of Invertible.t
  | Delay of Warp.Formal.t * Warp.Formal.t

let compare c1 c2 =
  let tag_to_int c =
    match c with
    | Seq _ -> 0
    | Arr _ -> 1
    | Prod _ -> 2
    | Warped _ -> 3
    | Invertible _ -> 4
    | Delay _ -> 5
  in
  match c1, c2 with
  | Seq (c1_1, c1_2), Seq (c2_1, c2_2)
  | Arr (c1_1, c1_2), Arr (c2_1, c2_2)
  | Prod (c1_1, c1_2), Prod (c2_1, c2_2)
    ->
     Warp.Utils.compare_both
       (compare c1_1 c2_1)
       (fun () -> compare c1_2 c2_2)
  | Warped (p1, c1), Warped (p2, c2) ->
     Warp.Utils.compare_both
       (Warp.Formal.compare p1 p2)
       (fun () -> compare c1 c2)
  | Invertible i1, Invertible i2 ->
     Invertible.compare i1 i2
  | Delay (p1, q1), Delay (p2, q2) ->
     Warp.Utils.compare_both
       (Warp.Formal.compare p1 p2)
       (fun () -> Warp.Formal.compare q1 q2)
  | (Seq _ | Arr _ | Prod _ | Warped _ | Invertible _ | Delay _), _ ->
     Warp.Utils.compare_int (tag_to_int c1) (tag_to_int c2)

let equal c1 c2 =
  compare c1 c2 = 0

let priority c =
  match c with
  | Invertible _ | Delay _ ->
     0
  | Warped _ ->
     10
  | Arr _ ->
     20
  | Prod _ ->
     30
  | Seq _ ->
     40

let rec print pri fmt c =
  let pri' = priority c in
  let print = print pri' in
  let paren = pri < pri' in
  if paren then Format.fprintf fmt "(@[";
  begin match c with
  | Seq (c1, c2) ->
     Format.fprintf fmt "@[%a;@ %a@]"
       print c1
       print c2
  | Arr (c1, c2) ->
     Format.fprintf fmt "@[%a %a@ %a@]"
       (print_under_arr pri) c1
       Warp.Print.pp_arrow ()
       print c2
  | Prod (c1, c2) ->
     Format.fprintf fmt "@[%a %a@ %a@]"
       print c1
       Warp.Print.pp_times ()
       print c2
  | Warped (p, c) ->
     Format.fprintf fmt "@[%a@ %a (@[<hov>%a@])@]"
       Warp.Formal.print p
       Warp.Print.pp_circledast ()
       print c
  | Invertible i ->
     Invertible.print fmt i
  | Delay (p, q) ->
     Format.fprintf fmt "@[delay %a %a@]"
       Warp.Formal.print p
       Warp.Formal.print q
  end;
  if paren then Format.fprintf fmt "@])"

and print_under_arr pri fmt c =
  match c with
  | Arr _ ->
     Format.fprintf fmt "(@[%a@])"
       (print pri) c
  | _ ->
     print pri fmt c

let print =
  print 500

let id =
  Invertible Invertible.Id

let rec try_invert c =
  match c with
  | Seq (c1, c2) ->
     Seq (try_invert c2, try_invert c1)
  | Prod (c1, c2) ->
     Prod (try_invert c1, try_invert c2)
  | Arr (c1, c2) ->
     Arr (try_invert c1, try_invert c2)
  | Warped (p, c) ->
     Warped (p, try_invert c)
  | Invertible i ->
     Invertible (Invertible.invert i)
  | Delay (p, q) ->
     if Warp.Formal.equal p q
     then c
     else raise (Invalid_argument "non-invertible delay coercion")

exception Ill_typed

let get_base ty =
  match ty with
  | Type.Base bty ->
     bty
  | _ ->
     raise Ill_typed

let get_fun ty =
  match ty with
  | Type.Fun (ty1, ty2) ->
     ty1, ty2
  | _ ->
     raise Ill_typed

let get_prod ty =
  match ty with
  | Type.Prod (ty1, ty2) ->
     ty1, ty2
  | _ ->
     raise Ill_typed

let get_warped ty =
  match ty with
  | Type.Warped (p, ty) ->
     p, ty
  | _ ->
     raise Ill_typed

let get_warped_check q ty =
  let p, ty = get_warped ty in
  if not (Warp.Formal.equal p q) then raise Ill_typed;
  ty

open Invertible

(* Typing *)

let output_type_invertible i ty =
  match i with
  | Id ->
     ty
  | Wrap ->
     Type.Warped (Warp.Formal.one, ty)
  | Unwrap ->
     get_warped_check Warp.Formal.one ty
  | Concat (p, q) ->
     let ty = get_warped_check p ty in
     let ty = get_warped_check q ty in
     Type.Warped (Warp.Formal.on p q, ty)
  | Decat (p, q) ->
     let ty = get_warped_check (Warp.Formal.on p q) ty in
     Type.Warped (p, Type.Warped (q, ty))
  | Dist ->
     let p, ty = get_warped ty in
     let ty1, ty2 = get_prod ty in
     Type.(Prod (Warped (p, ty1), Warped (p, ty2)))
  | Fact ->
     let ty1, ty2 = get_prod ty in
     let p, ty1 = get_warped ty1 in
     let q, ty2 = get_warped ty2 in
     Type.(Prod (Warped (p, ty1), Warped (p, ty2)))
  | Infl ->
     ignore @@ get_base ty;
     Type.Warped (Warp.Formal.omega, ty)
  | Defl ->
     let ty = get_warped_check Warp.Formal.omega ty in
     ignore @@ get_base ty;
     ty

let rec output_type c ty =
  match c with
  | Seq (c1, c2) ->
     output_type c2 (output_type c1 ty)
  | Arr (c1, c2) ->
     let ty1', ty2 = get_fun ty in
     Type.Fun (input_type c1 ty1', output_type c2 ty2)
  | Prod (c1, c2) ->
     let ty1, ty2 = get_prod ty in
     Type.Prod (output_type c1 ty1, output_type c2 ty2)
  | Warped (p, c) ->
     let ty = get_warped_check p ty in
     let ty = output_type c ty in
     Type.Warped (p, ty)
  | Invertible i ->
     output_type_invertible i ty
  | Delay (p, q) ->
     let ty = get_warped_check p ty in
     if not Warp.Formal.(q <= p) then raise Ill_typed;
     Type.Warped (q, ty)

and input_type c ty =
  match c with
  | Seq (c1, c2) ->
     input_type c1 (input_type c2 ty)
  | Arr (c1, c2) ->
     let ty1, ty2' = get_fun ty in
     Type.Fun (output_type c1 ty1, input_type c2 ty2')
  | Prod (c1, c2) ->
     let ty1', ty2' = get_prod ty in
     Type.Prod (input_type c1 ty1', input_type c2 ty2')
  | Warped (p, c) ->
     let ty = get_warped_check p ty in
     let ty = input_type c ty in
     Type.Warped (p, ty)
  | Invertible i ->
     output_type_invertible (invert i) ty
  | Delay (p, q) ->
     let ty = get_warped_check q ty in
     if not Warp.Formal.(q <= p) then raise Ill_typed;
     Type.Warped (p, ty)

(* Equational theory *)

let rec seq (c1, c2) =
  match c1, c2 with
  | Seq (c1, c2), c3 ->
     begin match seq (c2, c3) with
     | Seq (c2, c3) ->
        Seq (seq (c1, c2), c3)
     | c2 ->
        seq (c1, c2)
     end

  | c1, Seq (c2, c3) ->
     begin match seq (c1, c2) with
     | Seq (c1, c2) ->
        Seq (seq (c1, c2), c3)
     | c1 ->
        seq (c1, c3)
     end

  | Invertible Id, _ ->
     c2

  | _, Invertible Id ->
     c1

  | Prod (c11, c12), Prod (c21, c22) ->
     prod (seq (c11, c21), seq (c12, c22))

  | Arr (c11, c12), Arr (c21, c22) ->
     arr (seq (c21, c11), seq (c12, c22))

  | Invertible i, Invertible i' when Invertible.equal i (invert i') ->
     Invertible Id

  | _ ->
     Seq (c1, c2)

and seqs cs =
  match cs with
  | [] ->
     Invertible Id
  | [c] ->
     c
  | c :: cs ->
     seq (c, seqs cs)

and arr (c1, c2) =
  match c1, c2 with
  | Invertible Id, Invertible Id ->
     Invertible Id

  | _ ->
     Arr (c1, c2)

and prod (c1, c2) =
  match c1, c2 with
  | Invertible Id, Invertible Id ->
     Invertible Id

  | _ ->
     Prod (c1, c2)

and warped (p, c) =
  let open Warp.Formal in
  match c with
  | Invertible Id ->
     Invertible Id

  | Seq (c1, c2) ->
     seq (warped (p, c1), warped (p, c2))

  | Prod (c1, c2) ->
     seqs
       [
         invertible Dist;
         prod (warped (p, c1), warped (p, c2));
         invertible Fact;
       ]

  | Warped (q, c) ->
     seqs
       [
         invertible (Concat (p, q));
         warped (Warp.Formal.on p q, c);
         invertible (Decat (p, q));
       ]

  | Invertible Wrap ->
     invertible (Decat (p, one))

  | Invertible Unwrap ->
     invertible (Concat (p, one))

  | Invertible Defl ->
     seqs
       [
         invertible (Concat (p, omega));
         Delay (on p omega, p);
       ]

  | Delay (q, r) ->
     seqs
       [
         invertible (Concat (p, q));
         Delay (on p q, on p r);
         invertible (Decat (p, r));
       ]

  | _ ->
     if equal p one
     then seqs [invertible Unwrap; c; invertible Wrap]
     else Warped (p, c)

and invertible i =
  let open Warp.Formal in
  match i with
  | Concat (p, q) when equal p one ->
     Invertible Unwrap

  | Decat (p, q) when equal p one ->
     Invertible Wrap

  | _ ->
     Invertible i

and delay (p, q) =
  if Warp.Formal.equal p q
  then Invertible Id
  else Delay (p, q)

let rec reduce c =
  match c with
  | Seq (c1, c2) ->
     seq (reduce c1, reduce c2)
  | Arr (c1, c2) ->
     arr (reduce c1, reduce c2)
  | Prod (c1, c2) ->
     prod (reduce c1, reduce c2)
  | Warped (p, c) ->
     warped (p, reduce c)
  | Invertible i ->
     invertible i
  | Delay (p, q) ->
     delay (p, q)
