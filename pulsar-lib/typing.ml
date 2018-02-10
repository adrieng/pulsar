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

module E = Ident.Env
module S = Scoped_tree.T
module TT = Typed_tree
module T = TT.T

(* Errors *)

type expectation =
  | Exact of Type.t
  | Base
  | Stream
  | Prod
  | Fun
  | Sub of expectation

let rec print_expectation fmt ex =
  match ex with
  | Exact ty ->
     Format.fprintf fmt "type %a"
       Type.print ty
  | Base ->
     Format.fprintf fmt "(Base _)"
  | Stream ->
     Format.fprintf fmt "(Stream _)"
  | Prod ->
     Format.fprintf fmt "(_ %a _)"
       Warp.Print.pp_times ()
  | Fun ->
     Format.fprintf fmt "(_ %a _)"
       Warp.Print.pp_arrow ()
  | Sub ex ->
     Format.fprintf fmt "a type coercible to %a"
       print_expectation ex

type infer_kind =
  | Eq of Scoped_tree.T.eq
  | Pat of Scoped_tree.T.pat

let print_infer_kind fmt k =
  match k with
  | Eq eq ->
     Format.fprintf fmt "equation %a"
       Scoped_tree.T.print_eq eq
  | Pat p ->
     Format.fprintf fmt "pattern %a"
       Scoped_tree.T.print_pat p

let type_clash ~expected ~actual ~loc () =
  let body fmt () =
    Format.fprintf fmt "expected %a@ but got %a"
      print_expectation expected
      Type.print actual
  in
  Compiler.Diagnostic.error ~loc ~body ()

let cannot_infer ~kind ~loc () =
  let body fmt () =
    Format.fprintf fmt "cannot guess the type of %a"
      print_infer_kind kind
  in
  Compiler.Diagnostic.error ~loc ~body ()

let cannot_coerce ?id ~ty ~coe ~loc () =
  let body fmt () =
    Format.fprintf fmt "cannot apply coerci@[<v>on @[%a@]@ to @[%a@]"
      S.print_coe coe
      Type.print ty;
    begin match id with
    | None -> ()
    | Some id -> Format.fprintf fmt "@ applied on %a" Ident.print id
    end;
    Format.fprintf fmt "@]"
  in
  Compiler.Diagnostic.error ~loc ~body ()

let ill_typed_pat ~pat ~expected () =
  let body fmt () =
    Format.fprintf fmt "cannot type pat@[<v>tern @[%a@]@ with @[%a@]@]"
      S.print_pat pat
      Type.print expected
  in
  Compiler.Diagnostic.error ~loc:pat.S.p_loc ~body ()

let not_a_subtype ~ty1 ~ty2 ~clash_ty1 ~clash_ty2 loc =
  let body fmt () =
    Format.fprintf fmt
      "@[<hv>%a is not a subtype of %a"
      Type.print ty1
      Type.print ty2;
    if ty1 <> clash_ty1 || ty2 <> clash_ty2
    then
      Format.fprintf fmt
        "@ since@ %a is not a subtype of %a"
        Type.print clash_ty1
        Type.print clash_ty2;
    Format.fprintf fmt "@]"
  in
  Compiler.Diagnostic.error ~loc ~body ()

(* Debugging *)

let print_env fmt env =
  Format.fprintf fmt "[@[%a@]]"
    (E.print ~sep:"," Type.print) env

(* Utilities *)

let e_ty e =
  e.T.e_ann

let eq_ty eq =
  eq.T.eq_ann

let p_ty pat =
  pat.T.p_ann

let coe_src c =
  c.T.c_ann.src

let coe_dst c =
  c.T.c_ann.dst

let build_fun_ty tys =
  let rec loop acc tys =
    match tys with
    | [] ->
       acc
    | ty :: tys ->
       loop (Type.Fun (ty, acc)) tys
  in
  match tys with
  | [] ->
     invalid_arg "build_fun_ty: empty list"
  | ty :: tys ->
     loop ty tys

(* Coercion and subtyping *)

(* [is_shifted_omega p] is true iff p is of the form 0^d(\omega). *)
let is_shifted_omega p =
  (* (1)/p = d(0) iff p = 0^d(omega)  *)
  let open Warp.Enat in
  let open Warp.Formal in
  let q = div one p in
  eval q Inf = eval q (Fin 1)

let rec is_simplified ty =
  let open Type in
  match ty with
  | Warped (p, Base _) ->
     is_shifted_omega p
  | Warped (_, (Stream ty)) ->
     is_simplified ty
  | Warped (_, Fun (ty1, ty2)) | Prod (ty1, ty2) ->
     is_simplified ty1 && is_simplified ty2
  | _ ->
     false

(* [simplify_ty ty] always returns an invertible coercion from [ty] to its
   simplified form. *)
let rec simplify ty =
  let open Type in

  if is_simplified ty
  then TT.cid ty
  else
    match ty with
    | Base _ ->
       TT.cinvertible ty Infl

    | Stream ty ->
       let c = simplify ty in
       let ty' = c.T.c_ann.dst in
       TT.(cseq (cstream c, cinvertible (Stream ty') Wrap))

    | Prod (ty1, ty2) ->
       TT.cprod (simplify ty1, simplify ty2)

    | Fun (ty1, ty2) ->
       let c1 = simplify ty1 in
       let c2 = simplify ty2 in
       let c = TT.(carr (try_invert c1, c2)) in
       let i = TT.cinvertible c.T.c_ann.dst Wrap in
       TT.cseq (c, i)

    | Warped (p, (Prod (ty1, ty2))) ->
       let c = simplify (Prod (Warped (p, ty1), Warped (p, ty2))) in
       let i = TT.cinvertible ty Dist in
       TT.cseq (i, c)

    | Warped (p, ty) ->
       let c1 = simplify ty in
       let q, ty = Type.get_warped c1.T.c_ann.dst in
       let c1 = TT.cwarped (p, c1) in
       let c2 = TT.cinvertible c1.T.c_ann.dst (Concat (p, q)) in
       TT.cseq (c1, c2)

let simplify_ty ty =
  let c = simplify ty in
  c.T.c_ann.dst, c

let precedes_coe ~loc ~orig_ty1 ~orig_ty2 ty ty' =
  let not_a_subtype clash_ty1 clash_ty2 =
    not_a_subtype ~ty1:orig_ty1 ~ty2:orig_ty2 ~clash_ty1 ~clash_ty2 loc
  in
  let rec loop ty ty' =
    let open Type in
    if Type.equal ty ty'
    then TT.cid ty
    else
      match ty, ty' with
      | Stream ty, Stream ty' ->
         let c = loop ty ty' in
         TT.cstream c
      | Prod (ty1, ty2), Prod (ty1', ty2') ->
         let c1 = loop ty1 ty1' in
         let c2 = loop ty2 ty2' in
         TT.cprod (c1, c2)
      | Fun (ty1, ty2), Fun (ty1', ty2') ->
         let c1 = loop ty1' ty1 in
         let c2 = loop ty2 ty2' in
         TT.carr (c1, c2)
      | Warped (p, ty), Warped (q, ty') when Warp.Formal.(q <= p) ->
         let c = loop ty ty' in
         TT.(cseq (cwarped (p, c), cdelay ty' (p, q)))
      | _ ->
         not_a_subtype ty ty'
  in
  loop ty ty'

let subty_coe ~loc ty1 ty2 =
  let ty1', c1 = simplify_ty ty1 in
  let c2' = TT.try_invert @@ simplify ty2 in
  let ty2' = c2'.T.c_ann.src in
  assert (is_simplified ty1');
  assert (is_simplified ty2');
  let c3 = precedes_coe ~loc ~orig_ty1:ty1 ~orig_ty2:ty2 ty1' ty2' in
  Typed_tree.(cseq (cseq (c1, c3), c2'))

let coerce ~loc exp ty =
  let output = subty_coe ~loc (e_ty exp) ty in
  Typed_tree.sub ~output exp

let div_ctx env p =
  let rec div_ty ty =
    let open Type in
    match ty with
    | Warped (q, ty) ->
       (* q = p * (q / p) *)
       let q_div_p = Warp.Formal.div q p in
       let ty' = Warped (p, Warped (q_div_p, ty)) in
       subty_coe ~loc:Loc.nowhere (Warped (q, ty)) ty', Warped (q_div_p, ty)
    | Prod (ty1, ty2) ->
       let c1, ty1 = div_ty ty1 in
       let c2, ty2 = div_ty ty2 in
       TT.cprod (c1, c2), Prod (ty1, ty2)
    | _ ->
       assert false             (* not simplified? *)
  in

  let div_binding id ty (env, coes) =
    let ty, c1 = simplify_ty ty in
    let c2, ty = div_ty ty in
    Ident.Env.add id ty env,
    (id, TT.cseq (c1, c2)) :: coes
  in

  let env, coes = E.fold div_binding env (E.empty, []) in

  env, coes

(* [get_XXX] functions do not use subtyping. *)

let get_base loc actual =
  match actual with
  | Type.Base bty ->
     bty
  | _ ->
     type_clash ~expected:Base ~actual ~loc ()

let get_stream loc actual =
  match actual with
  | Type.Stream bty ->
     bty
  | _ ->
     type_clash ~expected:Stream ~actual ~loc ()

let get_prod loc actual =
  match actual with
  | Type.Prod (ty1, ty2) ->
     ty1, ty2
  | _ ->
     type_clash ~expected:Prod ~actual ~loc ()

(** [inv_XXX] functions solve inequations *)

let inv_fun e =
  let ty, _ = simplify_ty e.T.e_ann in
  match ty with
  | Type.(Warped (_, Fun (ty1, ty2))) ->
     (* We call coerce to avoid redoing coercion generation; note that it may
     fail if the warped type is not greater than (1). *)
     coerce ~loc:e.T.e_loc e (Type.Fun (ty1, ty2)), ty1, ty2
  | _ ->
     type_clash ~expected:(Sub Fun) ~actual:e.T.e_ann ~loc:e.T.e_loc ()

let inv_prod e =
  let ty, c1 = simplify_ty e.T.e_ann in
  match ty with
  | Type.Prod (ty1, ty2) as ty ->
     (* Same as above, but coerce never fails. *)
     coerce ~loc:e.T.e_loc e ty, ty1, ty2
  | _ ->
     type_clash ~expected:(Sub Prod) ~actual:e.T.e_ann ~loc:e.T.e_loc ()

let inv_base e =
  let ty, c1 = simplify_ty e.T.e_ann in
  match ty with
  | Type.(Warped (_, Base bty)) ->
     (* Same as above, coerce may fail as in [inv_fun]. *)
     coerce ~loc:e.T.e_loc e (Type.Base bty), bty
  | _ ->
     type_clash ~expected:(Sub Base) ~actual:e.T.e_ann ~loc:e.T.e_loc ()

let inv_stream e =
  let ty, c1 = simplify_ty e.T.e_ann in
  match ty with
  | Type.(Warped (_, Stream ty)) ->
     (* Same as above, coerce may fail as in [inv_fun]. *)
     coerce ~loc:e.T.e_loc e (Type.Stream ty), ty
  | _ ->
     type_clash ~expected:(Sub Stream) ~actual:e.T.e_ann ~loc:e.T.e_loc ()

(* Main code *)

(* TODO: there is no provision for applying coercions inside patterns, and thus
   the following two functions use exact type equality instead of
   subtyping. This restriction should be lifted in a future version. *)

let rec expect_pat p ty out_env =
  let out_env, pd =
    match p.S.p_desc with
    | S.PVar id ->
       E.add id ty out_env, T.PVar id
    | S.PPair (p1, p2) ->
       let ty1, ty2 = get_prod p.S.p_loc ty in
       let out_env, p1 = expect_pat p1 ty1 out_env in
       let out_env, p2 = expect_pat p2 ty2 out_env in
       out_env, T.PPair (p1, p2)
    | S.PCons (p1, p2) ->
       let ty = get_stream p.S.p_loc ty in
       let out_env, p1 = expect_pat p1 ty out_env in
       let out_env, p2 = expect_pat p2 Type.(later @@ Stream ty) out_env in
       out_env, T.PCons (p1, p2)
    | S.PAnnot (p, ty') ->
       if not (Type.equal ty ty')
       then type_clash ~expected:(Exact ty') ~actual:ty ~loc:p.S.p_loc ();
       let out_env, p = expect_pat p ty out_env in
       out_env, T.PAnnot (p, ty')
  in
  out_env,
  {
    T.p_desc = pd;
    T.p_loc = p.S.p_loc;
    T.p_ann = ty;
  }

let rec type_pat env p =
  let env, pd, ty =
    match p.S.p_desc with
    | S.PVar id ->
       cannot_infer ~kind:(Pat p) ~loc:p.S.p_loc ()

    | S.PPair (p1, p2) ->
       let env, p1 = type_pat env p1 in
       let env, p2 = type_pat env p2 in
       env, T.PPair (p1, p2), Type.Prod (p_ty p1, p_ty p2)

    | S.PCons (p1, p2) ->
       let env, p1 = type_pat env p1 in
       let ty = p_ty p1 in
       let env, p2 = expect_pat p2 Type.(later @@ Stream ty) env in
       env, T.PCons (p1, p2), Type.Stream ty

    | S.PAnnot (p, ty) ->
       let bound_env, p = expect_pat p ty env in
       E.merge_biased bound_env env, T.PAnnot (p, ty), ty
  in
  env,
  {
    T.p_desc = pd;
    T.p_loc = p.S.p_loc;
    T.p_ann = ty;
  }

let rec type_coe_fwd ?id src c =
  try
    let desc, dst =
      match c.S.c_desc, src with
      | S.CSeq (c1, c2), _ ->
         let c1 = type_coe_fwd ?id src c1 in
         let c2 = type_coe_fwd ?id (coe_dst c1) c2 in
         T.CSeq (c1, c2), coe_dst c2

      | S.CStream c, Type.Stream src ->
         let c = type_coe_fwd ?id src c in
         T.CStream c, Type.Stream (coe_dst c)

      | S.CProd (c1, c2), Type.Prod (src1, src2) ->
         let c1 = type_coe_fwd ?id src1 c1 in
         let c2 = type_coe_fwd ?id src2 c2 in
         T.CProd (c1, c2), Type.Prod (coe_dst c1, coe_dst c2)

      | S.CArr (c1, c2), Type.Fun (src1, src2)  ->
         let c1 = type_coe_bwd ?id src1 c1 in
         let c2 = type_coe_fwd ?id src2 c2 in
         T.CArr (c1, c2), Type.Fun (coe_src c1, coe_dst c2)

      | S.CWarped (p, c), Type.Warped (p', src) ->
         if not (Warp.Formal.equal p p') then invalid_arg "ill-typed";
         let c = type_coe_fwd ?id src c in
         T.CWarped (p, c), Type.Warped (p', coe_dst c)

      | S.CInvertible i, _ ->
         let dst = Invertible.dst_ty i src in
         T.CInvertible i, dst

      | S.CDelay (p, q), Type.Warped (p', src) ->
         if not (Warp.Formal.equal p p') then invalid_arg "ill-typed";
         if not Warp.Formal.(q <= p) then invalid_arg "ill-typed";
         T.CDelay (p, q), Type.Warped (q, src)

      | _ ->
         invalid_arg "ill-typed"
    in
    {
      T.c_desc = desc;
      T.c_loc = c.S.c_loc;
      T.c_ann = { src; dst; };
    }
  with Invertible.Ill_typed | Invalid_argument _ ->
    cannot_coerce ?id ~ty:src ~coe:c ~loc:c.S.c_loc ()

and type_coe_bwd ?id dst c =
  try
    let desc, src =
      match c.S.c_desc, dst with
      | S.CSeq (c1, c2), _ ->
         let c2 = type_coe_bwd ?id dst c2 in
         let c1 = type_coe_bwd ?id (coe_src c2) c1 in
         T.CSeq (c1, c2), coe_src c1

      | S.CStream c, Type.Stream src ->
         let c = type_coe_bwd ?id src c in
         T.CStream c, Type.Stream (coe_src c)

      | S.CProd (c1, c2), Type.Prod (dst1, dst2) ->
         let c1 = type_coe_bwd ?id dst1 c1 in
         let c2 = type_coe_bwd ?id dst2 c2 in
         T.CProd (c1, c2), Type.Prod (coe_src c1, coe_src c2)

      | S.CArr (c1, c2), Type.Fun (dst1, dst2)  ->
         let c1 = type_coe_fwd ?id dst1 c1 in
         let c2 = type_coe_bwd ?id dst2 c2 in
         T.CArr (c1, c2), Type.Fun (coe_dst c1, coe_src c2)

      | S.CWarped (p, c), Type.Warped (p', dst) ->
         if not (Warp.Formal.equal p p') then invalid_arg "ill-typed";
         let c = type_coe_bwd ?id dst c in
         T.CWarped (p, c), Type.Warped (p', coe_src c)

      | S.CInvertible i, _ ->
         let dst = Invertible.src_ty i dst in
         T.CInvertible i, dst

      | S.CDelay (p, q), Type.Warped (q', dst) ->
         if not (Warp.Formal.equal q q') then invalid_arg "ill-typed";
         if not Warp.Formal.(q <= p) then invalid_arg "ill-typed";
         T.CDelay (p, q), Type.Warped (p, dst)

      | _ ->
         invalid_arg "ill-typed"
    in
    {
      T.c_desc = desc;
      T.c_loc = c.S.c_loc;
      T.c_ann = { src; dst; };
    }
  with Invertible.Ill_typed | Invalid_argument _ ->
    (* FIXME The error message will not be right. *)
    cannot_coerce ?id ~ty:dst ~coe:c ~loc:c.S.c_loc ()

let bind_rec_eq out_env eq =
  let res_ty =
    match eq.S.eq_ty with
    | None -> cannot_infer ~kind:(Eq eq) ~loc:eq.S.eq_loc ()
    | Some res_ty -> res_ty
  in
  let _, params =
    Warp.Utils.mapfold_left type_pat E.empty eq.S.eq_params
  in
  let ty =
    Type.later @@ build_fun_ty (res_ty :: List.rev_map p_ty params)
  in
  fst @@ expect_pat eq.S.eq_lhs ty out_env

let rec type_exp env e =
  let coerce = coerce ~loc:e.S.e_loc in
  let ty, ed =
    match e.S.e_desc with
    | S.EVar id ->
       Ident.Env.find id env, T.EVar id

    | S.EExternal n ->
       assert false             (* TODO *)

    | S.ELam (p, e) ->
       let env, p = type_pat env p in
       let e = type_exp env e in
       Type.Fun (p_ty p, e_ty e), T.ELam (p, e)

    | S.EApp (e1, e2) ->
       let e1 = type_exp env e1 in
       let e1, t1, t2 = inv_fun e1 in
       let e2 = type_exp env e2 in
       let e2 = coerce e2 t1 in
       t2, T.EApp (e1, e2)

    | S.ECons (e1, e2) ->
       let e1 = type_exp env e1 in
       let ty = e1.T.e_ann in
       let e2 = type_exp env e2 in
       let e2 = coerce e2 Type.(later (Stream ty)) in
       Type.Stream ty, T.ECons (e1, e2)

    | S.EPair (e1, e2) ->
       let e1 = type_exp env e1 in
       let e2 = type_exp env e2 in
       Type.Prod (e_ty e1, e_ty e2), T.EPair (e1, e2)

    | S.EFst e ->
       let e = type_exp env e in
       let e, t1, _ = inv_prod e in
       t1, T.EFst e

    | S.ESnd e ->
       let e = type_exp env e in
       let e, _, t2 = inv_prod e in
       t2, T.ESnd e

    | S.ELet { block; body; } ->
       let bound_env, block = type_block env block in
       let body = type_exp (E.merge_biased bound_env env) body in
       e_ty body, T.ELet { block; body; }

    | S.EWhere { body; block; } ->
       let bound_env, block = type_block env block in
       let body = type_exp (E.merge_biased bound_env env) body in
       e_ty body, T.EWhere { body; block; }

    | S.EConst c ->
       Const.type_of c, T.EConst c

    | S.EBy { body; dr; } ->
       (* Weaken the environment to remove variables not free in body. *)
       let env = E.trim env @@ Scoped_tree.V.free_vars_exp body in
       let env, input = div_ctx env dr in
       let body = type_exp env body in
       let ty = Type.Warped (dr, e_ty body) in
       let exp =
         {
           T.e_desc = T.EBy { body; dr; };
           T.e_loc = e.S.e_loc;
           T.e_ann = ty;
         }
       in
       let exp = Typed_tree.sub ~input exp in
       ty, exp.T.e_desc

    | S.EAnnot { exp; kind; annot; } ->
       let exp = type_exp env exp in
       let exp = coerce exp annot in
       annot, T.EAnnot { exp; kind; annot; }

    | S.ESub { ctx; exp; res; } ->
       let apply_coe_env (env, ctx) (id, coe) =
         let ty = E.find id env in
         let coe = type_coe_fwd ~id ty coe in
         E.add id coe.T.c_ann.dst env,
         (id, coe) :: ctx
       in
       let env, ctx = List.fold_left apply_coe_env (env, []) ctx in
       let exp = type_exp env exp in
       let res = type_coe_fwd exp.T.e_ann res in
       res.T.c_ann.dst, T.ESub { ctx; exp; res; }
  in
  {
    T.e_desc = ed;
    T.e_loc = e.S.e_loc;
    T.e_ann = ty;
  }

and type_eq
      local_env                 (* env binding free vars of eq *)
      out_env                   (* env enriched with bound vars of eq *)
      eq =
  let local_env, params =
    Warp.Utils.mapfold_left type_pat local_env eq.S.eq_params
  in
  let rhs = type_exp local_env eq.S.eq_rhs in
  let rhs =
    match eq.S.eq_ty with
    | None -> rhs
    | Some ty -> coerce eq.S.eq_loc rhs ty
  in
  let ty = build_fun_ty (e_ty rhs :: List.rev_map p_ty params) in
  let out_env, lhs = expect_pat eq.S.eq_lhs ty out_env in
  out_env,
  {
    T.eq_lhs = lhs;
    T.eq_params = params;
    T.eq_ty = eq.S.eq_ty;
    T.eq_rhs = rhs;
    T.eq_loc = eq.S.eq_loc;
    T.eq_ann = ty;
  }

and type_block env { S.b_kind; S.b_body; S.b_loc; } =
  let bound_env, b_body =
    match b_kind with
    | Seq ->
       let type_eq bound_env eq =
         type_eq (E.merge_biased bound_env env) bound_env eq
       in
       Warp.Utils.mapfold_left type_eq E.empty b_body
    | Par ->
       let type_eq bound_env eq =
         type_eq (E.merge_biased bound_env env) bound_env eq
       in
       Warp.Utils.mapfold_left type_eq E.empty b_body
    | Rec ->
       let rec_env = List.fold_left bind_rec_eq env b_body in
       Warp.Utils.mapfold_left (type_eq rec_env) E.empty b_body
  in
  bound_env,
  {
    T.b_kind;
    T.b_body;
    T.b_loc;
  }

let type_phrase env phr =
  let env, pd =
    match phr.S.ph_desc with
    | S.PDef block ->
       let bound_env, block = type_block env block in
       let bound_env =
         E.map
           (fun ty -> if !Options.auto_const then Type.constant ty else ty)
           bound_env
       in
       E.merge_biased bound_env env, T.PDef block
    | S.PDecl { id; ty; } ->
       E.add id ty env, T.PDecl { id; ty; }
  in
  env,
  {
    T.ph_desc = pd;
    T.ph_loc = phr.S.ph_loc;
    T.ph_ann = ();
  }

let type_file file =
  let env, phrases =
    Warp.Utils.mapfold_left type_phrase E.empty file.S.f_phrases
  in
  {
    T.f_name = file.S.f_name;
    T.f_phrases = phrases;
    T.f_loc = file.S.f_loc;
    T.f_annot = env;
  }

let pass =
  Compiler.Pass.atomic
    ~pp_in:S.print_file
    ~pp_out:T.print_file
    ~name:"typing"
    type_file
