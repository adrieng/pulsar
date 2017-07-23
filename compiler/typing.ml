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
module T = Typed_tree.T

(* Errors *)

type expectation =
  | Exact of Type.t
  | Base
  | Stream
  | Prod
  | Fun
  | Sub of expectation

type infer_kind =
  | Eq of Scoped_tree.T.eq
  | Pat of Scoped_tree.T.pat

type typing_error =
  | Type_clash of { expected : expectation; actual : Type.t; loc : Loc.loc; }
  | Cannot_infer of { kind : infer_kind; loc : Loc.loc; }
  | Cannot_coerce of { ty : Type.t; coe : Coercion.t; loc : Loc.loc; }
  | Ill_typed_pat of { pat : Scoped_tree.T.pat; expected : Type.t; }
  | Not_a_subtype of { ty1 : Type.t; ty2 : Type.t;
                       clash_ty1 : Type.t; clash_ty2 : Type.t;
                       loc : Loc.loc; }

exception Typing_error of typing_error

let type_clash ~expected ~actual ~loc =
  raise (Typing_error (Type_clash { expected; actual; loc; }))

let cannot_infer ~kind ~loc =
  raise (Typing_error (Cannot_infer { kind; loc; }))

let cannot_coerce ~ty ~coe ~loc =
  raise (Typing_error (Cannot_coerce { ty; coe; loc; }))

let ill_typed_pat ~pat ~expected =
  raise (Typing_error (Ill_typed_pat { pat; expected; }))

let not_a_subtype ~ty1 ~ty2 ~clash_ty1 ~clash_ty2 loc =
  raise (Typing_error (Not_a_subtype { ty1; ty2; clash_ty1; clash_ty2; loc; }))

let rec print_expectation fmt e =
  match e with
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
  | Sub e ->
     Format.fprintf fmt "coercible to %a"
       print_expectation e

let print_infer_kind fmt k =
  match k with
  | Eq eq ->
     Format.fprintf fmt "equation %a"
       Scoped_tree.T.print_eq eq
  | Pat p ->
     Format.fprintf fmt "pattern %a"
       Scoped_tree.T.print_pat p

let print_typing_error fmt err =
  match err with
  | Type_clash { expected; actual; loc; } ->
     Format.fprintf fmt
       "@[<hv 2>%a: type error,@;expected a type %a but got %a@]"
       Loc.print_loc loc
       print_expectation expected
       Type.print actual
  | Cannot_infer { kind; loc; } ->
     Format.fprintf fmt
       "@[<hv 2>%a: type error,@;cannot guess the type of %a@]"
       Loc.print_loc loc
       print_infer_kind kind
  | Cannot_coerce { ty; coe; loc; } ->
     Format.fprintf fmt
       "@[<hv 2>%a: type error,@;cannot apply coercion %a to @[%a@]@]"
       Loc.print_loc loc
       Coercion.print coe
       Type.print ty
  | Ill_typed_pat { pat; expected; } ->
     Format.fprintf fmt
       "@[<hv 2>%a: type error,@;cannot type pattern %a with %a@]"
       Loc.print_loc pat.S.p_loc
       S.print_pat pat
       Type.print expected
  | Not_a_subtype { ty1; ty2; clash_ty1; clash_ty2; loc; } ->
     Format.fprintf fmt
       "@[<hv 2>%a: type error,@;@[<hv>%a@;is not a subtype of@;%a@]"
       Loc.print_loc loc
       Type.print ty1
       Type.print ty2;
     if ty1 <> clash_ty1 || ty2 <> clash_ty2
     then
       Format.fprintf fmt
         "@;since@;@[<hv>%a@;is not a subtype of@;%a@]"
         Type.print clash_ty1
         Type.print clash_ty2;
     Format.fprintf fmt "@]"

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

let rec is_simplified ty =
  let open Type in
  match ty with
  | Warped (_, (Base _ | Stream _)) ->
     true
  | Warped (_, Fun (ty1, ty2)) | Prod (ty1, ty2) ->
     is_simplified ty1 && is_simplified ty2
  | _ ->
     false

let rec simplify_ty ty =
  let open Coercion in
  let open Type in

  if is_simplified ty
  then ty, Id, Id
  else
    match ty with
    | Base _ ->
       Warped (Warp.Formal.omega, ty),
       invertible Infl,
       invertible Defl

    | Stream _ ->
       Warped (Warp.Formal.one, ty),
       invertible Wrap,
       invertible Unwrap

    | Prod (ty1, ty2) ->
       let ty1, c11, c12 = simplify_ty ty1 in
       let ty2, c21, c22 = simplify_ty ty2 in
       Prod (ty1, ty2),
       Coercion.prod (c11, c21),
       Coercion.prod (c21, c22)

    | Fun (ty1, ty2) ->
       let ty1, c11, c12 = simplify_ty ty1 in
       let ty2, c21, c22 = simplify_ty ty2 in
       Warped (Warp.Formal.one, Fun (ty1, ty2)),
       Coercion.(seq (arr (c12, c21), invertible Wrap)),
       Coercion.(seq (invertible Unwrap, arr (c11, c22)))

    | Warped (p, Prod (ty1, ty2)) ->
       let ty, c1, c2 = simplify_ty (Prod (Warped (p, ty1), Warped (p, ty2))) in
       ty,
       Coercion.(seq (invertible Dist, c1)),
       Coercion.(seq (c2, invertible Fact))

    | Warped (p, ty) ->
       let q, ty, c1, c2 =
         let ty, c1, c2 = simplify_ty ty in
         match ty with
         | Warped (q, ty) ->
            q, ty, c1, c2
         | _ ->
            assert false
       in
       Warped (Warp.Formal.on p q, ty),
       Coercion.(seq (warped (p, c1), invertible (Concat (p, q)))),
       Coercion.(seq (invertible (Decat (p, q)), warped (p, c2)))

let precedes_coe ~loc ~orig_ty1 ~orig_ty2 ty ty' =
  let not_a_subtype clash_ty1 clash_ty2 =
    not_a_subtype ~ty1:orig_ty1 ~ty2:orig_ty2 ~clash_ty1 ~clash_ty2 loc
  in
  let rec loop ty ty' =
    let open Type in
    if Type.equal ty ty'
    then Coercion.Id
    else
      match ty, ty' with
      | Prod (ty1, ty2), Prod (ty1', ty2') ->
         let c1 = loop ty1 ty1' in
         let c2 = loop ty2 ty2' in
         Coercion.prod (c1, c2)
      | Fun (ty1, ty2), Fun (ty1', ty2') ->
         let c1 = loop ty1' ty1 in
         let c2 = loop ty2 ty2' in
         Coercion.arr (c1, c2)
      | Warped (p, ty), Warped (q, ty') when Warp.Formal.(q <= p) ->
         let c = loop ty ty' in
         Coercion.(seq (warped (p, c), delay (p, q)))
      | _ ->
         not_a_subtype ty ty'
  in
  loop ty ty'

let subty_coe ~loc ty1 ty2 =
  let ty1', c1, _ = simplify_ty ty1 in
  let ty2', _, c2' = simplify_ty ty2 in
  assert (is_simplified ty1');
  assert (is_simplified ty2');
  let c3 = precedes_coe ~loc ~orig_ty1:ty1 ~orig_ty2:ty2 ty1' ty2' in
  Coercion.(seq (seq (c1, c3), c2'))

let coerce ~loc exp ty =
  let res = subty_coe ~loc (e_ty exp) ty in
  Typed_tree.coerce_with exp res ty

let div_ctx env p =
  let rec div_ty ty =
    let open Type in
    match ty with
    | Warped (q, ty) ->
       let q_div_p = Warp.Formal.div q p in
       Warped (q_div_p, ty),
       Coercion.(
         seqs
           [
             delay (q, Warp.Formal.on p q_div_p);
             invertible (Decat (p, q_div_p))
           ]
       )
    | Prod (ty1, ty2) ->
       let ty1, c1 = div_ty ty1 in
       let ty2, c2 = div_ty ty2 in
       Prod (ty1, ty2), Coercion.prod (c1, c2)
    | _ ->
       assert false             (* not simplified? *)
  in

  let div_binding id ty (env, coes) =
    let ty, c1, _ = simplify_ty ty in
    let ty, c2 = div_ty ty in
    Ident.Env.add id ty env,
    (id, Coercion.seq (c1, c2)) :: coes
  in

  let env, coes = E.fold div_binding env (E.empty, []) in

  env, coes

(* [get_XXX] functions do not use subtyping. *)

let get_base loc actual =
  match actual with
  | Type.Base bty ->
     bty
  | _ ->
     type_clash ~expected:Base ~actual ~loc

let get_stream loc actual =
  match actual with
  | Type.Stream bty ->
     bty
  | _ ->
     type_clash ~expected:Stream ~actual ~loc

let get_prod loc actual =
  match actual with
  | Type.Prod (ty1, ty2) ->
     ty1, ty2
  | _ ->
     type_clash ~expected:Prod ~actual ~loc

(** [inv_XXX] functions solve inequations *)

let inv_fun e =
  let ty, _, _ = simplify_ty e.T.e_ann in
  match ty with
  | Type.(Warped (_, Fun (ty1, ty2))) ->
     (* We call coerce to avoid redoing coercion generation; note that it may
     fail if the warped type is not greater than (1). *)
     coerce ~loc:e.T.e_loc e (Type.Fun (ty1, ty2)), ty1, ty2
  | _ ->
     type_clash ~expected:(Sub Fun) ~actual:e.T.e_ann ~loc:e.T.e_loc

let inv_prod e =
  let ty, c1, _ = simplify_ty e.T.e_ann in
  match ty with
  | Type.Prod (ty1, ty2) as ty ->
     (* Same as above, but coerce never fails. *)
     coerce ~loc:e.T.e_loc e ty, ty1, ty2
  | _ ->
     type_clash ~expected:(Sub Prod) ~actual:e.T.e_ann ~loc:e.T.e_loc

let inv_base e =
  let ty, c1, _ = simplify_ty e.T.e_ann in
  match ty with
  | Type.(Warped (_, Base bty)) ->
     (* Same as above, coerce may fail as in [inv_fun]. *)
     coerce ~loc:e.T.e_loc e (Type.Base bty), bty
  | _ ->
     type_clash ~expected:(Sub Base) ~actual:e.T.e_ann ~loc:e.T.e_loc

(* Main code *)

(* TODO: there is no provision for applying coercions inside patterns, and thus
the following two functions use exact type equality instead of subtyping. This
restriction should be lifted in a future version. *)

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
       let bty = get_stream p.S.p_loc ty in
       let out_env, p1 = expect_pat p1 (Type.Base bty) out_env in
       let out_env, p2 = expect_pat p2 Type.(later @@ Stream bty) out_env in
       out_env, T.PCons (p1, p2)
    | S.PAnnot (p, ty') ->
       if not (Type.equal ty ty')
       then type_clash ~expected:(Exact ty') ~actual:ty ~loc:p.S.p_loc;
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
       cannot_infer ~kind:(Pat p) ~loc:p.S.p_loc

    | S.PPair (p1, p2) ->
       let env, p1 = type_pat env p1 in
       let env, p2 = type_pat env p2 in
       env, T.PPair (p1, p2), Type.Prod (p_ty p1, p_ty p2)

    | S.PCons (p1, p2) ->
       let env, p1 = type_pat env p1 in
       let bty = get_base p.S.p_loc (p_ty p1) in
       let env, p2 = expect_pat p2 Type.(later @@ Stream bty) env in
       env, T.PCons (p1, p2), Type.Stream bty

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

let bind_rec_eq out_env eq =
  let res_ty =
    match eq.S.eq_ty with
    | None -> cannot_infer ~kind:(Eq eq) ~loc:eq.S.eq_loc
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
       let e1, bt = inv_base e1 in
       let e2 = type_exp env e2 in
       let e2 = coerce e2 Type.(later (Stream bt)) in
       Type.Stream bt, T.ECons (e1, e2)

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
       let env, ctx = div_ctx env dr in
       let body = type_exp env body in
       let ty = Type.Warped (dr, e_ty body) in
       let exp =
         {
           T.e_desc = T.EBy { body; dr; };
           T.e_loc = e.S.e_loc;
           T.e_ann = ty;
         }
       in
       ty, T.ESub { ctx; exp; res = Coercion.Id; }

    | S.EAnnot { exp; kind; annot; } ->
       let exp = type_exp env exp in
       let exp = coerce exp annot in
       annot, T.EAnnot { exp; kind; annot; }

    | S.ESub { ctx; exp; res; } ->
       let output_type coe ty =
         try Coercion.output_type coe ty
         with Coercion.Ill_typed ->
           cannot_coerce ~ty ~coe ~loc:e.S.e_loc
       in
       let apply_coe_env env (id, coe) =
         let ty = E.find id env in
         E.add id (output_type coe ty) env
       in
       let env = List.fold_left apply_coe_env env ctx in
       let exp = type_exp env e in
       let ty = output_type res (e_ty exp) in
       ty, T.ESub { ctx; exp; res; }
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
    T.f_annot = env;
  }

let pass =
  Compiler.Pass.atomic
    ~pp_in:S.print_file
    ~pp_out:T.print_file
    ~name:"typing"
    type_file

let serialize =
  let typing_serialize_file file =
    if !Options.display_types
    then
      Format.printf "%a@."
        Typed_tree.print_interface file.T.f_annot;
    file
  in
  Compiler.Pass.atomic
    ~pp_in:Warp.Print.pp_nothing
    ~pp_out:Warp.Print.pp_nothing
    ~name:"typing_serialize"
    typing_serialize_file
