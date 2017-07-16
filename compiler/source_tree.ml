type annot_kind =
  | Typing
  | Subtyping

let print_annot_kind fmt k =
  match k with
  | Typing ->
     Format.fprintf fmt ":"
  | Subtyping ->
     Format.fprintf fmt "<:"

let compare_annot_kind k1 k2 =
  let tag_to_int k =
    match k with
    | Typing -> 0
    | Subtyping -> 1
  in
  match k1, k2 with
  | Typing, Typing | Subtyping, Subtyping ->
     0
  | (Typing | Subtyping), _ ->
     Warp.Utils.compare_int (tag_to_int k1) (tag_to_int k2)

module type Info =
sig
  type id
  val print_id : Format.formatter -> id -> unit
  val compare_id : id -> id -> int

  type ann
  val print_ann : Format.formatter -> ann -> unit
  val compare_ann : ann -> ann -> int
end


module type Tree =
sig
  include Info

  type pat =
    {
      p_desc : pat_desc;
      p_loc : Loc.loc;
      p_ann : ann;
    }

  and pat_desc =
    | PVar of id
    | PPair of pat * pat
    | PCons of pat * pat
    | PAnnot of pat * Type.t

  type exp =
      {
        e_desc : exp_desc;
        e_loc : Loc.loc;
        e_ann : ann;
      }

  and exp_desc =
    | EVar of id
    | ELam of pat * exp
    | EApp of exp * exp
    | EPair of exp * exp
    | EFst of exp
    | ESnd of exp
    | EWhere of { body : exp; is_rec : bool; eqs : eq list; }
    | EConst of Const.const
    | EBy of { body : exp; dr : Warp_type.t; }
    | EAnnot of { exp : exp; kind : annot_kind; annot : Type.t; }
    | ESub of { ctx : (id * Coercions.t) list; exp : exp; res : Coercions.t; }

  and eq =
      {
        eq_lhs : pat;
        eq_params : pat list;
        eq_ty : Type.t option;
        eq_rhs : exp;
        eq_loc : Loc.loc;
      }

  val print_exp : Format.formatter -> exp -> unit

  val print_eq : Format.formatter -> eq -> unit

  val compare_exp : exp -> exp -> int

  val compare_eq : eq -> eq -> int

  type phr =
    | Def of { is_rec : bool; body : eq; }

  val print_phr : Format.formatter -> phr -> unit

  val compare_phr : phr -> phr -> int

  type file =
      {
        f_name : string;
        f_phrases : phr list;
      }

  val print_file : Format.formatter -> file -> unit

  val compare_file : file -> file -> int
end

module Make = functor (I : Info) ->
struct
  include I

  type pat =
    {
      p_desc : pat_desc;
      p_loc : Loc.loc;
      p_ann : ann;
    }

  and pat_desc =
    | PVar of id
    | PPair of pat * pat
    | PCons of pat * pat
    | PAnnot of pat * Type.t

  type exp =
      {
        e_desc : exp_desc;
        e_loc : Loc.loc;
        e_ann : ann;
      }

  and exp_desc =
    | EVar of I.id
    | ELam of pat * exp
    | EApp of exp * exp
    | EPair of exp * exp
    | EFst of exp
    | ESnd of exp
    | EWhere of { body : exp; is_rec : bool; eqs : eq list; }
    | EConst of Const.const
    | EBy of { body : exp; dr : Warp_type.t; }
    | EAnnot of { exp : exp; kind : annot_kind; annot : Type.t; }
    | ESub of { ctx : (id * Coercions.t) list; exp : exp; res : Coercions.t; }

  and eq =
      {
        eq_lhs : pat;
        eq_params : pat list;
        eq_ty : Type.t option;
        eq_rhs : exp;
        eq_loc : Loc.loc;
      }

  let rec print_pat fmt p =
    match p.p_desc with
    | PVar x ->
       print_id fmt x
    | PPair (p1, p2) ->
       Format.fprintf fmt "(@[%a,@;%a@])"
         print_pat p1
         print_pat p2
    | PCons (p1, p2) ->
       Format.fprintf fmt "(@[%a ::@;%a@])"
         print_pat p1
         print_pat p2
    | PAnnot (p, ty) ->
       Format.fprintf fmt "(@[%a :@;%a@])"
         print_pat p
         Type.print ty

  let rec print_exp_prio prio fmt e =
    let open Warp.Print in
    match e.e_desc with
    | EVar x ->
      print_id fmt x

    | ELam (p, e) ->
      Format.fprintf fmt "@[<hov 2>%a %a %a@ %a@]"
        Pp.print_lam ()
        print_pat p
        Pp.print_warr ()
        print_exp e

    | EApp ({ e_desc =
                EApp ({ e_desc = EConst Const.Op op; _ }, e1); _ }, e2) ->
       let prio' = Const.priority op in
       if prio' < prio
       then
         Format.fprintf fmt "(@[%a@])"
           (print_exp_prio prio') e
       else
         Format.fprintf fmt "@[<hov 2>%a@ %a %a@]"
           (print_exp_prio prio') e1
           Const.print_op op
           (print_exp_prio prio') e2

    | EApp (e1, e2) ->
      Format.fprintf fmt "@[<hov 2>%a %a@]"
        print_exp e1
        print_exp_app e2

    | EPair (e1, e2) ->
      Format.fprintf fmt "(@[<v>%a,@ %a@])"
        print_exp e1
        print_exp e2

    | EWhere { body; is_rec; eqs; } ->
      Format.fprintf fmt "@[%a where%s@ {@[<v 2>%a@]}@]"
        print_exp body
        (if is_rec then " rec" else "")
        (pp_list ~pp_sep:pp_semicolon print_eq) eqs

    | EFst e ->
      Format.fprintf fmt "@[fst@ %a@]"
        print_exp e

    | ESnd e ->
      Format.fprintf fmt "@[snd@ %a@]"
        print_exp e

    | EConst c ->
      Const.print_const fmt c

    | EBy { body; dr; } ->
      Format.fprintf fmt "@[%a@ by %a@]"
        print_exp_simple body
        Warp_type.print dr

    | EAnnot { exp = e; kind = a; annot = ty; } ->
      Format.fprintf fmt "@[%a@ %a %a@]"
        print_exp e
        print_annot_kind a
        Type.print ty

    | ESub { ctx; exp; res; } ->
       let print_ident_coercion fmt (id, c) =
         Format.fprintf fmt "(%a <<@ %a)"
           print_id id
           Coercions.print c
       in
       Format.fprintf fmt "@[<b>{!@[<2>@[%a@]@ >> %a@ >> %a @]!}@]"
         (pp_list
            ~pp_left:pp_breakable_space
            ~pp_sep:pp_comma
            print_ident_coercion) ctx
         print_exp exp
         Coercions.print res

  and print_exp_simple fmt e =
    match e.e_desc with
    | EVar _ | EConst (Const.Lit _) ->
       print_exp fmt e
    | _ ->
       Format.fprintf fmt "(%a)"
         print_exp e

  and print_exp_app fmt e =
    match e.e_desc with
    | EApp (e1, e2) ->
       Format.fprintf fmt "%a@ %a"
         print_exp e1
         print_exp_app e2
    | _ ->
       print_exp fmt e

  and print_exp fmt e =
    print_exp_prio 0 fmt e

  and print_eq fmt { eq_lhs; eq_params; eq_ty; eq_rhs; _ } =
    let print_res_ty =
      Warp.Print.pp_opt
        ~pp_left:Warp.Print.pp_breakable_space
        (fun fmt ty -> Format.fprintf fmt ": %a" Type.print ty)
    in
    Format.fprintf fmt "@[%a%a%a @,= %a@]@]"
      print_pat eq_lhs
      Warp.Print.(pp_list ~pp_left:pp_breakable_space print_pat) eq_params
      print_res_ty eq_ty
      print_exp eq_rhs

  let rec compare_pat (p1 : pat) (p2 : pat) =
    if p1 == p2 then 0
    else
      let tag_to_int (pd : pat_desc) =
        match pd with
        | PVar _ -> 0
        | PPair _ -> 1
        | PCons _ -> 2
        | PAnnot _ -> 3
      in
      match p1.p_desc, p2.p_desc with
      | PVar v1, PVar v2 ->
         compare_id v1 v2
      | PPair (p11, p12), PPair (p21, p22)
      | PCons (p11, p12), PCons (p21, p22) ->
         Warp.Utils.compare_both
           (compare_pat p11 p21)
           (fun () -> compare_pat p12 p22)
      | PAnnot (p1, ty1), PAnnot (p2, ty2) ->
         Warp.Utils.compare_both
           (compare_pat p1 p2)
           (fun () -> Type.compare ty1 ty2)
      | (PVar _ | PPair _ | PCons _ | PAnnot _), _ ->
         Warp.Utils.compare_int (tag_to_int p1.p_desc) (tag_to_int p2.p_desc)

  let rec compare_exp e1 e2 =
    if e1 == e2 then 0 else compare_exp_desc e1.e_desc e2.e_desc

  and compare_exp_desc ed1 ed2 =
    if ed1 == ed2 then 0
    else
      let tag_to_int ed =
        match ed with
        | EVar _ -> 0
        | ELam _ -> 1
        | EApp _ -> 2
        | EPair _ -> 3
        | EFst _ -> 4
        | ESnd _ -> 5
        | EWhere _ -> 6
        | EConst _ -> 7
        | EBy _ -> 8
        | EAnnot _ -> 9
        | ESub _ -> 10
      in
      match ed1, ed2 with
      | EVar v1, EVar v2 ->
        compare_id v1 v2
      | ELam (p, e), ELam (p', e') ->
        Warp.Utils.compare_both
          (compare_pat p p')
          (fun () -> compare_exp e e')
      | EApp (e1, e2), EApp (e1', e2')
      | EPair (e1, e2), EPair (e1', e2') ->
         Warp.Utils.compare_both
           (compare_exp e1 e1')
           (fun () -> compare_exp e2 e2')
      | EFst e, EFst e' | ESnd e, ESnd e' ->
        compare_exp e e'
      | EWhere { body = e1; is_rec = r1; eqs = eqs1; },
        EWhere { body = e2; is_rec = r2; eqs = eqs2; } ->
        Warp.Utils.compare_both
          (Warp.Utils.compare_bool r1 r2)
          (fun () ->
            Warp.Utils.compare_both
              (compare_exp e1 e2)
              (fun () -> Warp.Utils.compare_list compare_eq eqs1 eqs2))
      | EConst c, EConst c' ->
        Const.compare_const c c'
      | EBy { body = e1; dr = p1; },
        EBy { body = e2; dr = p2; } ->
        Warp.Utils.compare_both
          (Warp_type.compare p1 p2)
          (fun () ->
            compare_exp e1 e2)
      | EAnnot { exp = e1; kind = k1; annot = ty1; },
        EAnnot { exp = e2; kind = k2; annot = ty2; } ->
         Warp.Utils.compare_both
           (compare_annot_kind k1 k2)
           (fun () ->
             Warp.Utils.compare_both
               (Type.compare ty1 ty2)
               (fun () ->
                 compare_exp e1 e2))
      | ESub { ctx = ctx1; exp = exp1; res = res1; },
        ESub { ctx = ctx2; exp = exp2; res = res2; } ->
         let compare_ident_coercion (v1, c1) (v2, c2) =
           Warp.Utils.compare_both
             (compare_id v1 v2)
             (fun () -> Coercions.compare c1 c2)
         in
         Warp.Utils.compare_both
           (compare_exp exp1 exp2)
           (fun () ->
             Warp.Utils.compare_both
               (Coercions.compare res1 res2)
               (fun () ->
                 Warp.Utils.compare_list compare_ident_coercion ctx1 ctx2))
      | (EVar _ | ELam _ | EApp _ | EPair _ | EFst _ | ESnd _ | EWhere _
         | EConst _ | EBy _ | EAnnot _ | ESub _), _ ->
        Warp.Utils.compare_int (tag_to_int ed1) (tag_to_int ed2)

  and compare_eq
      { eq_lhs = p1; eq_ty = ty1; eq_rhs = e1; }
      { eq_lhs = p2; eq_ty = ty2; eq_rhs = e2; } =
    Warp.Utils.compare_both
      (compare_pat p1 p2)
      (fun () ->
        Warp.Utils.compare_both
          (Warp.Utils.compare_opt Type.compare ty1 ty2)
          (fun () -> compare_exp e1 e2))

  type phr =
    | Def of { is_rec : bool; body : eq; }

  let print_phr fmt phr =
    match phr with
    | Def { is_rec; body; } ->
      Format.fprintf fmt "@[let%s %a@]"
        (if is_rec then " rec" else "")
        print_eq body

  let compare_phr phr1 phr2 =
    if phr1 == phr2 then 0
    else
      match phr1, phr2 with
      | Def { is_rec = r1; body = b1 },
        Def { is_rec = r2; body = b2 } ->
         Warp.Utils.compare_both
           (Warp.Utils.compare_bool r1 r2)
           (fun () -> compare_eq b1 b2)

  type file =
      {
        f_name : string;
        f_phrases : phr list;
      }

  let print_file fmt { f_name; f_phrases; } =
    Format.fprintf fmt "(* File \"%s\" *)@\n" f_name;
    List.iter (fun phr -> Format.fprintf fmt "@\n%a@\n" print_phr phr) f_phrases

  let compare_file
      { f_name = n1; f_phrases = body1; }
      { f_name = n2; f_phrases = body2; } =
    Warp.Utils.compare_both
      (Warp.Utils.compare_string n1 n2)
      (fun () -> Warp.Utils.compare_list compare_phr body1 body2)
end
