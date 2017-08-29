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

let invertible ?(loc = Loc.nowhere) src i =
  let dst = Invertible.dst_ty i src in
  {
    c_desc = CInvertible i;
    c_loc = loc;
    c_ann = { src; dst; };
  }

let id ?loc ty =
  invertible ?loc ty Invertible.Id

let seq (c1, c2) =
  let src, _ = ty_of c1 in
  let _, dst = ty_of c2 in
  let open CoeAnnot in
  {
    c_desc = CSeq (c1, c2);
    c_loc = Loc.join c1.c_loc c2.c_loc;
    c_ann = { src; dst; };
  }

let prod (c1, c2) =
  let src1, dst1 = ty_of c1 in
  let src2, dst2 = ty_of c2 in
  {
    c_desc = CProd (c1, c2);
    c_loc = Loc.join c1.c_loc c2.c_loc;
    c_ann = { src = Type.Prod (src1, src2); dst = Type.Prod (dst1, dst2); };
  }

let arr (c1, c2) =
  let src1, dst1 = ty_of c1 in
  let src2, dst2 = ty_of c2 in
  {
    c_desc = CArr (c1, c2);
    c_loc = Loc.join c1.c_loc c2.c_loc;
    c_ann = { src = Type.Fun (dst1, src2); dst = Type.Fun (src1, dst2); };
  }

let warped (p, c) =
  let src, dst = ty_of c in
  let src = Type.Warped (p, src) in
  let dst = Type.Warped (p, dst) in
  {
    c_desc = CWarped (p, c);
    c_loc = c.c_loc;
    c_ann = { src; dst; };
  }

let delay ty (p, q) =
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
    | Some c, Some c' -> Some (seq (c, c'))
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

(** {3 Expressions} *)

let sub ?(ctx' = []) ?res' e ty =
  let res' =
    match res' with
    | None -> id ty
    | Some res' -> res'
  in
  let open T in
  let ctx' = simplify_ctx ctx' in
  match ctx', res'.c_desc, e.e_desc with
  | [], CInvertible Invertible.Id, _ ->
     e
  | _, _, ESub { ctx; exp; res; } ->
     let ctx = seq_ctx ctx ctx' in
     let res = seq (res, res') in
     if ctx <> [] || not (is_id res)
     then { e with e_desc = ESub { ctx; exp; res; }; }
     else exp
  | _ ->
     {
       e_desc = T.ESub { ctx = ctx'; exp = e; res = res'; };
       e_loc = e.e_loc;
       e_ann = ty;
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
