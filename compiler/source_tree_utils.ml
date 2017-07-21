module Vars(T : Source_tree.Tree with type Id.t = Ident.t) =
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
      | EVar id ->
         S.singleton id
      | EExternal _ | EConst _ ->
         S.empty
      | ELam (p, e) ->
         Ident.Set.diff (free_vars_exp e) (vars_pat p)
      | EApp (e1, e2) | ECons (e1, e2) | EPair (e1, e2) ->
         S.union (free_vars_exp e1) (free_vars_exp e2)
      | EFst exp | ESnd exp | EBy { body = exp; _ }
      | EAnnot { exp; _ } | ESub { exp; _ } ->
         free_vars_exp exp
      | ELet { block; body; } | EWhere { body; block; } ->
         let locals, vars = free_vars_block block in
         S.union vars (S.diff (free_vars_exp body) locals)

    and free_vars_eq { eq_params; eq_rhs; _ } =
      let locals = vars_pats eq_params in
      S.diff (free_vars_exp eq_rhs) locals

    and free_vars_block { b_kind; b_body; } =
      let free_vars_eqs eqs =
        let union vars eq = S.union vars @@ free_vars_eq eq in
        List.fold_left union S.empty eqs
      in

      let bound_vars_block () =
        vars_pats (List.map (fun eq -> eq.eq_lhs) b_body)
      in

      let open Source_tree.BlockKind in
      match b_kind with
      | Seq ->
         let add_eq (bound, free) eq =
           let new_bound = vars_pat eq.eq_lhs in
           S.union bound new_bound, S.diff (free_vars_eq eq) bound
         in
         List.fold_left add_eq (S.empty, S.empty) b_body

      | Par ->
         bound_vars_block (), free_vars_eqs b_body

      | Rec ->
         let bound_vars = bound_vars_block () in
         bound_vars, S.diff (free_vars_eqs b_body) bound_vars
  end
