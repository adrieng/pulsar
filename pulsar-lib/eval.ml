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

open Warp
open Typed_tree.T

(** {2 Values and Environments} *)

type value =
  | Vnil
  | Vconst of Const.const
  | Vscons of value * value
  | Vpair of value * value
  | Vclo of pat * exp * env
  | Vwarp of Formal.t * value
  | Vthunk of exp * env

and env =
  value Ident.Env.t

let rec print_value fmt v =
  match v with
  | Vnil ->
     Format.fprintf fmt "nil"

  | Vconst c ->
     Const.print_const fmt c

  | Vscons (v1, v2) ->
     Format.fprintf fmt "(@[%a ::@ %a@])"
                    print_value v1
                    print_value v2

  | Vpair (v1, v2) ->
     Format.fprintf fmt "(@[%a,@ %a@])"
                    print_value v1
                    print_value v2

  | Vclo (p, e, env) ->
     Format.fprintf
       fmt "(%a.%a)[%a]"
       print_pat p
       print_exp e
       print_env env

  | Vwarp (p, v) ->
     Format.fprintf fmt "%a%a%a"
                    Formal.print p
                    Print.pp_circledast ()
                    print_value v

  | Vthunk (e, env) ->
     Format.fprintf
       fmt "(%a)[%a]"
       print_exp e
       print_env env

and print_env fmt env =
  Ident.Env.print ~sep:";" print_value fmt env

(** {2 Errors} *)

type tag_error =
  | TE_coercion of coe * value
  | TE_pattern of pat * value
  | TE_expression of exp * string

exception Tag_error of tag_error

let print_tag_error fmt err =
  match err with
  | TE_coercion (coe, v) ->
     Format.fprintf
       fmt "@[<v 2>Cannot apply coercion@ @[%a@] to value@ @[%a@]@]"
       print_coe coe
       print_value v

  | TE_pattern (p, v) ->
     Format.fprintf
       fmt "@[<v 2>Cannot match pattern@ @[%a@] with value@ @[%a@]@]"
       print_pat p
       print_value v

  | TE_expression (e, msg) ->
     Format.fprintf
       fmt "@[<v 2>Ill-typed term@ @[%a@]:@ %s@]"
       print_exp e
       msg

let tag_error_coercion coe v =
  raise (Tag_error (TE_coercion (coe, v)))

let tag_error_pattern pat v =
  raise (Tag_error (TE_pattern (pat, v)))

let tag_error_expression exp msg =
  raise (Tag_error (TE_expression (exp, msg)))

(* {2 Utilities} *)

let rec extend env p v =
  match p.p_desc, v with
  | PVar x, _ ->
     Ident.Env.add x v env

  | PPair (p1, p2), Vpair (v1, v2)
  | PCons (p1, p2), Vscons (v1, v2)
    ->
     extend (extend env p1 v1) p2 v2

  | PAnnot (p, _), _ ->
     extend env p v

  | _ ->
     tag_error_pattern p v

let rec extend_nil env p =
  match p.p_desc with
  | PVar x ->
     Ident.Env.add x Vnil env

  | PPair (p1, p2) | PCons (p1, p2) ->
     extend_nil (extend_nil env p1) p2

  | PAnnot (p, _) ->
     extend_nil env p


let rec extend env p v =
  match p.p_desc, v with
  | PVar x, _ ->
     Ident.Env.add x v env

  | PPair (p1, p2), Vpair (v1, v2)
  | PCons (p1, p2), Vscons (v1, v2)
    ->
     extend (extend env p1 v1) p2 v2

  | PAnnot (p, _), _ ->
     extend env p v

  | _ ->
     tag_error_pattern p v

let project_env p env =
  let project_val x v env =
    match v with
    | Vwarp (p', v) when Warp.Formal.equal p p' ->
       Ident.Env.add x v env
    | _ ->
       env
  in
  Ident.Env.fold project_val env Ident.Env.empty

(** {2 Evaluation} *)

let eval_inv n c i v =
  assert (n > 0);
  let open Invertible in
  match i, v with
  | Id, _ ->
     v

  | Wrap, _ ->
     Vwarp (Formal.one, v)

  | Unwrap, Vwarp (one', v) when Formal.(equal one one') ->
     v

  | Concat (p, q), Vwarp (p', Vnil) when Formal.equal p p' ->
     Vwarp (Formal.on p q, Vnil)

  | Concat (p, q), Vwarp (p', Vwarp (q', v))
       when Formal.(equal p p' && equal q q')  ->
     Vwarp (Formal.on p q, v)

  | Decat (p, q), Vwarp (pq, v) when Formal.(equal (on p q) pq) ->
     v

  | Dist, Vwarp (p, v) when (Formal.eval p (Enat.Fin n) = Enat.Fin 0) ->
     Vpair (Vwarp (p, Vnil), Vwarp (p, Vnil))

  | Dist, Vwarp (p, Vpair (v1, v2)) ->
     Vpair (Vwarp (p, v1), Vwarp (p, v2))

  | Fact, Vpair (Vwarp (p, v1), Vwarp (p', v2)) when Formal.equal p p' ->
     if (Formal.eval p (Enat.Fin n) = Enat.Fin 0)
     then Vwarp (p, Vnil)
     else Vwarp (p, Vpair (v1, v2))

  | _ ->
     tag_error_coercion { c with c_desc = CInvertible i; } v

let rec restrict n v =
  let open Warp in
  match n with
  | Enat.Fin 0 ->
     Vnil

  | Enat.Fin m ->
     assert (m > 0);
     begin match v with
     | Vnil ->
        Vnil
     | Vconst c ->
        Vconst c
     | Vscons (v1, v2) ->
        Vscons (restrict n v1, restrict (Enat.Fin (m - 1)) v2)
     | Vpair (v1, v2) ->
        Vpair (restrict n v1, restrict n v2)
     | Vclo (p, e, env) ->
        Vclo (p, e, restrict_env n env)
     | Vwarp (p, v) ->
        Vwarp (p, restrict (Formal.eval p n) v)
     | Vthunk (e, env) ->
        eval_exp n e env
     end

  | Enat.Inf ->
     v

and restrict_env n env =
  Ident.Env.map (restrict n) env

and eval_coe n c v =
  let open Enat in
  match n with
  | Fin 0 ->
     Vnil

  | Fin m ->
     begin match c.c_desc, v with
     | CSeq (c1, c2), _ ->
        eval_coe n c2 (eval_coe n c1 v)

     | CArr (c1, c2), Vclo (p, e, env) ->
        assert false

     | CProd (c1, c2), Vpair (v1, v2) ->
        Vpair (eval_coe n c1 v1, eval_coe n c2 v2)

     | CWarped (p, c), Vwarp (p', v) when Formal.equal p p' ->
        Vwarp (p, eval_coe (Formal.eval p n) c v)

     | CInvertible i, _ ->
        eval_inv m c i v

     | CDelay (p, q), v ->
        assert (Formal.eval q n <= Formal.eval p n);
        restrict (Formal.eval q n) v

     | _ ->
        tag_error_coercion c v
     end

  | Inf ->
     assert false

and eval_coe_env n coe_env env =
  let coerce_val x v env =
    let c = List.assoc x coe_env in
    let v = eval_coe n c v in
    Ident.Env.add x v env
  in
  Ident.Env.fold coerce_val env Ident.Env.empty

and eval_exp n (e : exp) env =
  match n with
  | Enat.Fin 0 ->
     Vnil

  | Enat.Inf ->
     Vthunk (e, env)

  | Enat.Fin n' ->
     assert (n' > 0);
     begin match e.e_desc with
     | EVar x ->
        Ident.Env.find x env

     | EExternal _ ->
        assert false

     | ELam (p, e) ->
        Vclo (p, e, env)

     | EApp (e1, e2) ->
        let v1 = eval_exp n e1 env in
        begin match v1 with
        | Vclo (p, e, env') ->
           let v2 = eval_exp n e2 env in
           let env = restrict_env n env in
           let env = extend env p v2 in
           eval_exp n e env
        | _ ->
           tag_error_expression e1 "function expected"
        end

     | ECons (e1, e2) ->
        let v1 = eval_exp n e1 env in
        let v2 = eval_exp n e2 env in
        Vscons (v1, v2)

     | EPair (e1, e2) ->
        let v1 = eval_exp n e1 env in
        let v2 = eval_exp n e2 env in
        Vpair (v1, v2)

     | EFst e ->
        let v = eval_exp n e env in
        begin match v with
        | Vpair (v1, _) ->
           v1
        | _ ->
           tag_error_expression e "pair expected"
        end

     | ESnd e ->
        let v = eval_exp n e env in
        begin match v with
        | Vpair (_, v2) ->
           v2
        | _ ->
           tag_error_expression e "pair expected"
        end

     | ELet { block; body; } | EWhere { block; body; } ->
        let env = eval_block n block env in
        eval_exp n body env

     | EConst c ->
        Vconst c

     | EBy { body; dr; } ->
        let n = Warp.Formal.eval dr n in
        let env = project_env dr env in
        Vwarp (dr, eval_exp n body env)

     | EAnnot { exp; _ } ->
        eval_exp n exp env

     | ESub { ctx; exp; res; } ->
        let env = eval_coe_env n ctx env in
        let v = eval_exp n e env in
        eval_coe n res v
     end

and eval_block n block env =
  let open Source_tree.BlockKind in
  match block.b_kind with
  | Seq ->
     List.fold_left
       (fun env -> eval_eq n ~i_env:env ~e_env:env)
       env
       block.b_body

  | Par ->
     List.fold_left
       (fun e_env -> eval_eq n ~i_env:env ~e_env)
       env
       block.b_body

  | Rec ->
     begin match n with
     | Enat.Fin n ->
        let i_env =
          List.fold_left
            (fun env eq -> extend_nil env eq.eq_lhs)
            Ident.Env.empty
            block.b_body
        in
        let block = { block with b_kind = Par; } in
        iter_block n 0 ~e_env:env ~i_env block

     | Enat.Inf ->
        assert false
     end

and eval_eq n ~i_env ~e_env eq =
  let v = eval_exp n eq.eq_rhs i_env in
  extend e_env eq.eq_lhs v

and iter_block n m ~e_env ~i_env block =
  assert (m <= n);
  if m = n
  then Ident.Env.merge_biased ~winner:i_env ~loser:e_env
  else
    let e_env' = restrict_env (Enat.Fin m) e_env in
    let e_env' = Ident.Env.merge_biased ~winner:i_env ~loser:e_env' in
    let i_env = eval_block (Enat.Fin m) block e_env' in
    iter_block (n + 1) m ~e_env ~i_env block
