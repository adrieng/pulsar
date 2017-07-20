module E = Warp.Utils.Env
module R = Raw_tree.T
module S = Scoped_tree.T

type scoping_error =
  | Unbound_identifier of string * Loc.loc

exception Scoping_error of scoping_error

let unbound_identifier id loc =
  raise (Scoping_error (Unbound_identifier (id, loc)))

let print_scoping_error fmt err =
  match err with
  | Unbound_identifier (id, loc) ->
     Format.fprintf fmt "Unbound identifier %s at %a"
       id
       Loc.print_loc loc

let rec scope_pat env p =
  let env, pd =
    match p.R.p_desc with
    | R.PVar s ->
       let id = Ident.make_source s in
       E.add s id env, S.PVar id
    | R.PPair (p1, p2) ->
       let env, p1 = scope_pat env p1 in
       let env, p2 = scope_pat env p2 in
       env, S.PPair (p1, p2)
    | R.PCons (p1, p2) ->
       let env, p1 = scope_pat env p1 in
       let env, p2 = scope_pat env p2 in
       env, S.PCons (p1, p2)
    | R.PAnnot (p, ann) ->
       let env, p = scope_pat env p in
       env, S.PAnnot (p, ann)
  in
  env,
  {
    S.p_desc = pd;
    S.p_loc = p.R.p_loc;
    S.p_ann = ();
  }

let rec scope_exp env e =
  let ed =
    match e.R.e_desc with
    | R.EVar id ->
       begin
         try S.EVar (E.find id env)
         with Not_found -> unbound_identifier id e.R.e_loc
       end
    | R.EExternal n ->
       S.EExternal n
    | R.ELam (p, e) ->
       let env, p = scope_pat env p in
       S.ELam (p, scope_exp env e)
    | R.EApp (e1, e2) ->
       S.EApp (scope_exp env e1, scope_exp env e2)
    | R.ECons (e1, e2) ->
       S.ECons (scope_exp env e1, scope_exp env e2)
    | R.EPair (e1, e2) ->
       S.EPair (scope_exp env e1, scope_exp env e2)
    | R.EFst e ->
       S.EFst (scope_exp env e)
    | R.ESnd e ->
       S.ESnd (scope_exp env e)
    | R.EWhere { body; is_rec; eqs; } ->
       let env, eqs =
         Warp.Utils.mapfold_left (scope_eq is_rec) env eqs
       in
       S.EWhere { body = scope_exp env body; is_rec; eqs; }
    | R.EConst c ->
       S.EConst c
    | R.EBy { body; dr; } ->
       S.EBy { body = scope_exp env body; dr; }
    | R.EAnnot { exp; kind; annot; } ->
       S.EAnnot { exp = scope_exp env exp; kind; annot; }
    | R.ESub { ctx; exp; res; } ->
       let scope_coe (id, coe) =
         try E.find id env, coe
         with Not_found -> unbound_identifier id e.R.e_loc
       in
       S.ESub { ctx = List.map scope_coe ctx; exp = scope_exp env exp; res; }
  in
  {
    S.e_desc = ed;
    S.e_loc = e.R.e_loc;
    S.e_ann = ();
  }

and scope_eq is_rec prev_env eq =
  let env, lhs = scope_pat prev_env eq.R.eq_lhs in
  let local_env = if is_rec then env else prev_env in
  let local_env, params =
    Warp.Utils.mapfold_left scope_pat local_env eq.R.eq_params
  in
  let rhs = scope_exp local_env eq.R.eq_rhs in
  env,
  {
    S.eq_lhs = lhs;
    S.eq_params = params;
    S.eq_ty = eq.R.eq_ty;
    S.eq_rhs = rhs;
    S.eq_loc = eq.R.eq_loc;
    S.eq_ann = ();
  }

let scope_phrase env phr =
  let env, pd =
    match phr.R.ph_desc with
    | R.PDef { is_rec; body; } ->
       let env, body = scope_eq is_rec env body in
       env, S.PDef { is_rec; body; }
    | R.PDecl { id = s; ty; } ->
       let id = Ident.make_source s in
       E.add s id env, S.PDecl { id; ty; }
  in
  env,
  {
    S.ph_desc = pd;
    S.ph_loc = phr.R.ph_loc;
    S.ph_ann = ();
  }

let scope_file ctx file =
  Ident.reset_ctx ();
  let _, phrases =
    Warp.Utils.mapfold_left scope_phrase E.empty file.R.f_phrases
  in
  {
    S.f_name = file.R.f_name;
    S.f_phrases = phrases;
  }

let pass =
  Pass.atomic
    ~pp_in:Raw_tree.T.print_file
    ~pp_out:Scoped_tree.T.print_file
    ~name:"scoping"
    scope_file
