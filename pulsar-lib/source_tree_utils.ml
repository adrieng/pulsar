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

module Vars(T : Source_tree_sig.Tree with type Id.t = Ident.t) =
  struct
    open T
    module S = Ident.Set

    let rec vars_pat p =
      match p.p_desc with
      | PVar id ->
         S.singleton id
      | PPair (p1, p2) | PCons (p1, p2) ->
         S.union (vars_pat p1) (vars_pat p2)
      | PAnnot (p, _) ->
         vars_pat p

    let vars_pats ps =
      let union vars p = S.union vars @@ vars_pat p in
      List.fold_left union S.empty ps

    let rec free_vars_exp e =
      match e.e_desc with
      | EVar (VLocal id) ->
         S.singleton id
      | EVar (VExternal _) | EConst _ ->
         S.empty
      | ELam (p, e) ->
         Ident.Set.diff (free_vars_exp e) (vars_pat p)
      | EApp (e1, e2) | ECons (e1, e2) | EPair (e1, e2) ->
         S.union (free_vars_exp e1) (free_vars_exp e2)
      | EBy { body = exp; _ } | EAnnot { exp; _ } | ESub { exp; _ } ->
         free_vars_exp exp
      | ELet { block; body; } | EWhere { body; block; } ->
         let locals, vars = free_vars_block block in
         S.union vars (S.diff (free_vars_exp body) locals)

    and free_vars_eq { eq_params; eq_rhs; _ } =
      let locals = vars_pats eq_params in
      S.diff (free_vars_exp eq_rhs) locals

    and free_vars_block { b_kind; b_body; _ } =
      let free_vars_eqs eqs =
        let union vars eq = S.union vars @@ free_vars_eq eq in
        List.fold_left union S.empty eqs
      in

      let bound_vars_block () =
        vars_pats (List.map (fun eq -> eq.eq_lhs) b_body)
      in

      let open Source_tree_annot.BlockKind in
      match b_kind with
      | Seq ->
         let add_eq (bound, occ) eq =
           let new_bound = vars_pat eq.eq_lhs in
           let new_occ = free_vars_eq eq in
           S.union bound new_bound, S.union new_occ occ
         in
         let bound, uses = List.fold_left add_eq (S.empty, S.empty) b_body in
         bound, S.diff uses bound

      | Par ->
         bound_vars_block (), free_vars_eqs b_body

      | Rec ->
         let bound_vars = bound_vars_block () in
         bound_vars, S.diff (free_vars_eqs b_body) bound_vars
  end

module Sub(T : Source_tree_sig.Tree) =
struct
  open T

  let sub_pat p =
    match p.p_desc with
    | PVar _ ->
       []
    | PPair (p1, p2) | PCons (p1, p2) ->
       [ `Pat p1; `Pat p2; ]
    | PAnnot (p, _) ->
       [ `Pat p ]

  let sub_coe c =
    match c.c_desc with
    | CInvertible _ | CDelay _ ->
       []
    | CSeq (c1, c2) | CProd (c1, c2) | CArr (c1, c2) ->
       [ `Coe c1; `Coe c2; ]
    | CStream c | CWarped (_, c) ->
       [ `Coe c ]

  let sub_exp e =
    match e.e_desc with
    | EVar _ | EConst _ ->
       []
    | ELam (p, e) ->
       [ `Pat p; `Exp e; ]
    | EApp (e1, e2) | ECons (e1, e2) | EPair (e1, e2) ->
       [ `Exp e1; `Exp e2; ]
    | EAnnot { exp = e; _ } | EBy { body = e; _ } ->
       [ `Exp e; ]
    | ELet { block; body; } | EWhere { body; block; } ->
       [ `Exp body; `Block block; ]
    | ESub { ctx; exp; res; } ->
       List.map (fun (_, coe) -> `Coe coe) ctx
       @ [ `Exp exp; `Coe res; ]

  let sub_eq eq =
    `Pat eq.eq_lhs
    :: List.map (fun p -> `Pat p) eq.eq_params
    @ [ `Exp eq.eq_rhs; ]

  let sub_block block =
    List.map (fun eq -> `Eq eq) block.b_body

  let sub_phrase ph =
    match ph.ph_desc with
    | PDef block ->
       [ `Block block; ]
    | PDecl _ ->
       []

  let sub_file file =
    List.map (fun ph -> `Phr ph) file.f_phrases

  let sub thing =
    match thing with
    | `Pat p -> sub_pat p
    | `Coe c -> sub_coe c
    | `Exp e -> sub_exp e
    | `Block b -> sub_block b
    | `Eq eq -> sub_eq eq
    | `Phr ph -> sub_phrase ph
    | `File file -> sub_file file

  let sub_file file =
    List.map (fun phr -> `Phr phr) file.f_phrases
end

module Find(T : Source_tree_sig.Tree) =
struct
  module S = Sub(T)

  open T

  let print_thing fmt thing =
    match thing with
    | `Pat p ->
       print_pat fmt p
    | `Coe c ->
       print_coe fmt c
    | `Exp e ->
       print_exp fmt e
    | `Block b ->
       print_block fmt b
    | `Eq eq ->
       print_eq fmt eq
    | `Phr ph ->
       print_phr fmt ph

  let loc thing =
    match thing with
    | `Pat p -> p.p_loc
    | `Coe c -> c.c_loc
    | `Exp e -> e.e_loc
    | `Block b -> b.b_loc
    | `Eq eq -> eq.eq_loc
    | `Phr ph -> ph.ph_loc

  let rec find_in_things ?default pos things =
    match things with
    | [] ->
       begin match default with
       | Some def ->
          def
       | None ->
          raise Not_found
       end
    | thing :: things ->
       try find_in_thing pos thing
       with Not_found -> find_in_things ?default pos things

  and find_in_thing pos thing =
    if not (Loc.is_in (loc thing) pos) then raise Not_found;
    find_in_things ~default:thing pos @@ S.sub thing

  let find_in_file pos file =
    find_in_things pos @@ S.sub_file file
end

module Misc(T : Source_tree_sig.Tree) =
struct
  open T

  let loc thing =
    match thing with
    | `Pat p -> p.p_loc
    | `Coe c -> c.c_loc
    | `Exp e -> e.e_loc
    | `Block b -> b.b_loc
    | `Eq eq -> eq.eq_loc
    | `Phr ph -> ph.ph_loc
    | `File file -> file.f_loc
end
