module E = Warp.Utils.Env
module R = Raw_tree.T
module S = Scoped_tree.T

(* Error handling *)

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

(* Debugging *)

let print_env fmt env =
  Format.fprintf fmt "[@[%a@]]"
    (E.print ~sep:";" Warp.Print.pp_string Ident.print) env

(* Main code *)

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
    | R.ELet { block; body; } ->
       let env, block = scope_block env block in
       S.ELet { block; body = scope_exp env body; }
    | R.EWhere { body; block; } ->
       let env, block = scope_block env block in
       S.EWhere { body = scope_exp env body; block; }
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

and scope_eq env lhs eq =
  let local_env, params =
    Warp.Utils.mapfold_left scope_pat env eq.R.eq_params
  in
  let rhs = scope_exp local_env eq.R.eq_rhs in
  {
    S.eq_lhs = lhs;
    S.eq_params = params;
    S.eq_ty = eq.R.eq_ty;
    S.eq_rhs = rhs;
    S.eq_loc = eq.R.eq_loc;
    S.eq_ann = ();
  }

and scope_block env { b_kind; b_body; b_loc; } =
  let per_eq_envs, lhss =
    let add envs eq =
      let new_env, pat = scope_pat env eq.R.eq_lhs in
      new_env :: envs, pat
    in
    Warp.Utils.mapfold_left add [] b_body
  in
  let new_env =
    let merge_right loser winner = E.merge_biased ~winner ~loser in
    List.fold_left merge_right env per_eq_envs
  in
  let per_eq_envs =
    match b_kind with
    | Seq ->
       let add_merge (current_env, per_eq_envs) new_env =
         let new_env = E.merge_biased ~winner:new_env ~loser:current_env in
         (new_env, current_env :: per_eq_envs)
       in
       List.(rev @@ snd @@ fold_left add_merge (env, []) @@ rev per_eq_envs)
    | Par ->
       List.map (fun _ -> env) per_eq_envs
    | Rec ->
       List.map (fun _ -> new_env) per_eq_envs
  in
  new_env,
  {
    S.b_kind;
    S.b_body = Warp.Utils.map3 scope_eq per_eq_envs lhss b_body;
    S.b_loc;
  }

let scope_phrase env phr =
  let env, pd =
    match phr.R.ph_desc with
    | R.PDef block ->
       let env, block = scope_block env block in
       env, S.PDef block
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
