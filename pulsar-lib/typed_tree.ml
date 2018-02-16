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

module CoeAnnot =
struct
  type t = { src : Type.t; dst : Type.t; }

  let print fmt { src; dst; } =
    Format.fprintf fmt "%a@ <: %a"
      Type.print src
      Type.print dst

  let compare { src = src1; dst = dst1; } { src = src2; dst = dst2; } =
    Warp.Utils.(compare_both
                  (Type.compare src1 src2)
                  (fun () -> Type.compare dst1 dst2))
end

module T = Source_tree.Make(
  struct
    module Id =
      struct
        type t = Ident.t
        let print = Ident.print_source
        let compare = Ident.compare
      end

    module PatAnnot = Type
    module CoeAnnot = CoeAnnot
    module ExpAnnot = Type
    module EquAnnot = Type
    module PhrAnnot = Warp.Utils.PrintableOrderedUnit
    module FileAnnot =
      struct
        type t = Type.t Ident.Env.t
        let print = Ident.Env.print Type.print
        let compare _ _ = 0
      end
  end
)

(** {2 Various utilities and smart constructors} *)

open T

(** {3 Coercions} *)

let ty_of coe = coe.c_ann.src, coe.c_ann.dst

let is_id coe = coe.c_desc = CInvertible Invertible.Id

(** {4 Smart Constructors} *)

let rec shrink_seq c1 c2 =
  let open Invertible in
  match c1.c_desc, c2.c_desc with
  | CSeq (c11, c12), _ ->
     (cseq (c11, cseq (c12, c2))).c_desc

  (* c ; id = id ; c = c *)
  | CInvertible Id, _ ->
     c2.c_desc
  | _, CInvertible Id ->
     c1.c_desc

  (* c; c^-1 = id *)
  | CInvertible i1, CInvertible i2 when equal i1 (invert i2) ->
     CInvertible Id

  | CStream c, CStream c' ->
     (cstream (cseq (c, c'))).c_desc

  (* (c1 x c2);(c1' x c2') = (c1;c1') x (c2;c2') *)
  | CProd (c1, c2), CProd (c1', c2') ->
     (cprod (cseq (c1, c1'), cseq (c2, c2'))).c_desc

  (* (c1 -> c2);(c1' -> c2') = (c1' ; c1) -> (c2 ; c2') *)
  | CArr (c1, c2), CArr (c1', c2') ->
     (carr (cseq (c1', c1), cseq (c2, c2'))).c_desc

  | CDelay (p, q), CDelay (q', r) ->
     assert (Warp.Formal.equal q q'); (* Ill-typed? *)
     CDelay (p, r)

  | _ ->
     CSeq (c1, c2)

and shrink_stream c =
  match c.c_desc with
  | CInvertible Id ->
     CInvertible Id

  | _ ->
     CStream c

and shrink_prod c1 c2 =
  match c1.c_desc, c2.c_desc with
  | CInvertible Id, CInvertible Id ->
     CInvertible Id

  | _ ->
     CProd (c1, c2)

and shrink_arr c1 c2 =
  match c1.c_desc, c2.c_desc with
  | CInvertible Id, CInvertible Id ->
     CInvertible Id

  | _ ->
     CArr (c1, c2)

and shrink_warp p c =
  let open Invertible in
  let open Warp.Formal in
  let src = Type.Warped (p, c.c_ann.src) in
  match c.c_desc with
  | CInvertible Id ->
     CInvertible Id

  | CSeq (c1, c2) ->
     (cseq (cwarped (p, c1), cwarped (p, c2))).c_desc

  | CProd (c1, c2) ->
     let c1 = cinvertible src Dist in
     let c2 = cprod (cwarped (p, c1), cwarped (p, c2)) in
     let c3 = cinvertible c2.c_ann.dst Fact in
     (cseqs [c1; c2; c3]).c_desc

  | CWarped (q, c) ->
     let c1 = cinvertible src (Concat (p, q)) in
     let c2 = cwarped (on p q, c) in
     let c3 = cinvertible c2.c_ann.dst (Decat (p, q)) in
     (cseqs [c1; c2; c3]).c_desc

  | CInvertible Wrap ->
     (cinvertible src (Decat (p, one))).c_desc

  | CInvertible Unwrap ->
     (cinvertible src (Concat (p, one))).c_desc

  | CInvertible Defl ->
     let c1 = cinvertible src (Concat (p, omega)) in
     let c2 = cdelay c1.c_ann.dst (on p omega, p) in
     (cseqs [c1; c2]).c_desc

  | CDelay (q, r) ->
     let c1 = cinvertible src (Concat (p, q)) in
     let c2 = cdelay c1.c_ann.dst (on p q, on p r) in
     let c3 = cinvertible c2.c_ann.dst (Decat (p, r)) in
     (cseqs [c1; c2; c3]).c_desc

  | _ ->
     if equal p one
     then
       (cseqs [cinvertible src Unwrap; c; cinvertible c.c_ann.dst Wrap]).c_desc
     else CWarped (p, c)

and cinvertible ?(loc = Loc.nowhere) src i =
  let dst = Invertible.dst_ty i src in
  {
    c_desc = CInvertible i;
    c_loc = loc;
    c_ann = { src; dst; };
  }

and cid ?loc ty =
  cinvertible ?loc ty Invertible.Id

and cseq (c1, c2) =
  let src, _ = ty_of c1 in
  let _, dst = ty_of c2 in
  let open CoeAnnot in
  {
    c_desc = if !Options.auto_shrink then shrink_seq c1 c2 else CSeq (c1, c2);
    c_loc = Loc.join c1.c_loc c2.c_loc;
    c_ann = { src; dst; };
  }

and cseqs cs =
  match cs with
  | [] ->
     invalid_arg "cseqs: empty list"
  | [c] ->
     c
  | c1 :: c2 :: cs ->
     cseqs (cseq (c1, c2) :: cs)

and cstream c =
  let src, dst = ty_of c in
  {
    c_desc = if !Options.auto_shrink then shrink_stream c else CStream c;
    c_loc = c.c_loc;
    c_ann = { src = Type.Stream src; dst = Type.Stream dst; };
  }

and cprod (c1, c2) =
  let src1, dst1 = ty_of c1 in
  let src2, dst2 = ty_of c2 in
  {
    c_desc = if !Options.auto_shrink then shrink_prod c1 c2 else CProd (c1, c2);
    c_loc = Loc.join c1.c_loc c2.c_loc;
    c_ann = { src = Type.Prod (src1, src2); dst = Type.Prod (dst1, dst2); };
  }

and carr (c1, c2) =
  let src1, dst1 = ty_of c1 in
  let src2, dst2 = ty_of c2 in
  {
    c_desc = if !Options.auto_shrink then shrink_arr c1 c2 else CArr (c1, c2);
    c_loc = Loc.join c1.c_loc c2.c_loc;
    c_ann = { src = Type.Fun (dst1, src2); dst = Type.Fun (src1, dst2); };
  }

and cwarped (p, c) =
  let src, dst = ty_of c in
  let src = Type.Warped (p, src) in
  let dst = Type.Warped (p, dst) in
  {
    c_desc = if !Options.auto_shrink then shrink_warp p c else CWarped (p, c);
    c_loc = c.c_loc;
    c_ann = { src; dst; };
  }

and cdelay ty (p, q) =
  let src = Type.Warped (p, ty) in
  let dst = Type.Warped (q, ty) in
  {
    c_desc = CDelay (p, q);
    c_loc = Loc.nowhere;
    c_ann = { src; dst; };
  }

let seq_ctx ctx ctx' =
  let ctx = Ident.Env.of_list ctx in
  let ctx' = Ident.Env.of_list ctx' in
  let seq id c c' =
    match c, c' with
    | None, None -> None
    | Some c, None | None, Some c -> Some c
    | Some c, Some c' -> Some (cseq (c, c'))
  in
  List.filter (fun (v, c) -> c.c_desc <> CInvertible Invertible.Id)
  @@ Ident.Env.to_list
  @@ Ident.Env.merge seq ctx ctx'

let simplify_ctx ctx =
  seq_ctx ctx []

let rec try_invert c =
  let src, dst = ty_of c in
  let desc =
    match c.c_desc with
    | CSeq (c1, c2) ->
       CSeq (try_invert c2, try_invert c1)
    | CStream c ->
       CStream (try_invert c)
    | CProd (c1, c2) ->
       CProd (try_invert c1, try_invert c2)
    | CArr (c1, c2) ->
       CArr (try_invert c1, try_invert c2)
    | CWarped (p, c) ->
       CWarped (p, try_invert c)
    | CInvertible i ->
       CInvertible (Invertible.invert i)
    | CDelay (p, q) ->
       if Warp.Formal.equal p q
       then CDelay (p, q)
       else invalid_arg "non-invertible delay coercion"
  in
  {
    c_desc = desc;
    c_loc = Loc.nowhere;
    c_ann = { src = dst; dst = src; };
  }

(** {3 Patterns} *)

let pvar ?(loc = Loc.nowhere) id ty =
  {
    p_desc = T.PVar id;
    p_loc = loc;
    p_ann = ty;
  }

(** {3 Expressions} *)

let var ?(loc = Loc.nowhere) id ty =
  {
    e_desc = T.(EVar (VLocal id));
    e_loc = loc;
    e_ann = ty;
  }

let lam ?(loc = Loc.nowhere) p e =
  {
    e_desc = T.ELam (p, e);
    e_loc = loc;
    e_ann = Type.Fun (p.p_ann, e.e_ann);
  }

let app ?(loc = Loc.nowhere) e1 e2 =
  let ty =
    match e1.e_ann with
    | Type.Fun (_, ty) ->
       ty
    | _ ->
       invalid_arg "app"
  in
  {
    e_desc = T.EApp (e1, e2);
    e_loc = loc;
    e_ann = ty;
  }

let sub ?(input = []) ?output e =
  let output =
    match output with
    | None -> cid e.e_ann
    | Some output -> output
  in
  let open T in
  let input = simplify_ctx input in
  match input, output.c_desc, e.e_desc with
  | [], CInvertible Invertible.Id, _ ->
     e
  | _, _, ESub { ctx; exp; res; } ->
     let ctx = seq_ctx ctx input in
     let res = cseq (res, output) in
     if ctx <> [] || not (is_id res)
     then { e with e_desc = ESub { ctx; exp; res; }; }
     else exp
  | _ ->
     {
       e_desc = T.ESub { ctx = input; exp = e; res = output; };
       e_loc = e.e_loc;
       e_ann = output.c_ann.dst;
     }

(** {3 Files} *)

let print_interface fmt file =
  let print_binding fmt (k, v) =
    Format.fprintf fmt "@[val %a@ : %a@]"
      Ident.print_source k
      Type.print v
  in
  Format.fprintf fmt "@[<v>%a@]"
    (Warp.Print.pp_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@;@;")
       print_binding)
    (Ident.Env.to_list file)
