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

open Source_tree_annot
open Source_tree_sig

module Make = functor (I : Info) ->
struct
  include I

  type pat =
    {
      p_desc : pat_desc;
      p_loc : Loc.t;
      p_ann : PatAnnot.t;
    }

  and pat_desc =
    | PVar of Id.t
    | PPair of pat * pat
    | PCons of pat * pat
    | PAnnot of pat * Type.t

  type coe =
    {
      c_desc : coe_desc;
      c_loc : Loc.t;
      c_ann : CoeAnnot.t;
    }

  and coe_desc =
    | CSeq of coe * coe
    | CStream of coe
    | CArr of coe * coe
    | CProd of coe * coe
    | CWarped of Warp.Formal.t * coe
    | CInvertible of Invertible.t
    | CDelay of Warp.Formal.t * Warp.Formal.t

  type var =
    | VLocal of Id.t
    | VExternal of Name.t

  type exp =
      {
        e_desc : exp_desc;
        e_loc : Loc.t;
        e_ann : ExpAnnot.t;
      }

  and exp_desc =
    | EVar of var
    | ELam of pat * exp
    | EApp of exp * exp
    | ECons of exp * exp
    | EPair of exp * exp
    | ELet of { block : block; body : exp; }
    | EWhere of { body : exp; block : block; }
    | EConst of Const.const
    | EBy of { body : exp; dr : Warp.Formal.t; }
    | EAnnot of { exp : exp; kind : AnnotKind.t; annot : Type.t; }
    | ESub of { ctx : (Id.t * coe) list; exp : exp; res : coe; }

  and eq =
      {
        eq_lhs : pat;
        eq_params : pat list;
        eq_ty : Type.t option;
        eq_rhs : exp;
        eq_loc : Loc.t;
        eq_ann : EquAnnot.t;
      }

  and block =
    {
      b_kind : BlockKind.t;
      b_body : eq list;
      b_loc : Loc.t;
    }

  let rec print_pat fmt p =
    match p.p_desc with
    | PVar x ->
       Id.print fmt x
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


  let priority c =
    match c.c_desc with
    | CInvertible _ | CDelay _ ->
       0
    | CWarped _ ->
       10
    | CStream _ ->
       20
    | CArr _ ->
       30
    | CProd _ ->
       40
    | CSeq _ ->
       50

  let rec print_coe pri fmt c =
    let pri' = priority c in
    let print = print_coe pri' in
    let paren = pri < pri' in
    if paren then Format.fprintf fmt "(@[";
    begin match c.c_desc with
    | CSeq (c1, c2) ->
       Format.fprintf fmt "@[%a;@ %a@]"
         print c1
         print c2
    | CStream c ->
       Format.fprintf fmt "@[stream@ %a@]"
         print c
    | CArr (c1, c2) ->
       Format.fprintf fmt "@[%a %a@ %a@]"
         (print_under_arr pri') c1
         Warp.Print.pp_arrow ()
         print c2
    | CProd (c1, c2) ->
       Format.fprintf fmt "@[%a %a@ %a@]"
         print c1
         Warp.Print.pp_times ()
         print c2
    | CWarped (p, c) ->
       Format.fprintf fmt "@[%a@ %a @[<hov>%a@]@]"
         Warp.Formal.print p
         Warp.Print.pp_circledast ()
         print c
    | CInvertible i ->
       Invertible.print fmt i
    | CDelay (p, q) ->
       Format.fprintf fmt "@[delay %a %a@]"
         Warp.Formal.print p
         Warp.Formal.print q
    end;
    if paren then Format.fprintf fmt "@])"

  and print_under_arr pri fmt c =
    match c.c_desc with
    | CArr _ ->
       Format.fprintf fmt "(@[%a@])"
         (print_coe pri) c
    | _ ->
       print_coe pri fmt c

  let print_coe =
    print_coe 500

  let print_var fmt v =
    match v with
    | VLocal id ->
       Id.print fmt id

    | VExternal n ->
       Name.print fmt n

  let rec print_exp_prio prio fmt e =
    let open Warp.Print in
    match e.e_desc with
    | EVar v ->
       print_var fmt v

    | ELam (p, e) ->
      Format.fprintf fmt "@[<hov 2>%a %a %a@ %a@]"
        Warp.Print.pp_lambda ()
        print_pat p
        Warp.Print.pp_thick_arrow ()
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
      Format.fprintf fmt "@[<hov 2>%a@ %a@]"
        print_exp_app e1
        print_exp_simple e2

    | ECons (e1, e2) ->
      Format.fprintf fmt "@[<hov>%a ::@ %a@]"
        print_exp e1
        print_exp e2

    | EPair (e1, e2) ->
      Format.fprintf fmt "(@[<v>%a,@ %a@])"
        print_exp e1
        print_exp e2

    | ELet { body; block; } ->
      Format.fprintf fmt "@[let %a in@ %a@]"
        print_block block
        print_exp body

    | EWhere { body; block; } ->
      Format.fprintf fmt "@[%a@ where %a@]"
        print_exp body
        print_block block

    | EConst c ->
      Const.print_const fmt c

    | EBy { body; dr; } ->
      Format.fprintf fmt "@[%a@ by %a@]"
        print_exp_simple body
        Warp.Formal.print dr

    | EAnnot { exp = e; kind = a; annot = ty; } ->
      Format.fprintf fmt "@[%a@ %a %a@]"
        print_exp e
        AnnotKind.print a
        Type.print ty

    | ESub { ctx; exp; res; } ->
       let print_var_coercion fmt (id, c) =
         Format.fprintf fmt "@[<hov 2>%a <<@ %a@]"
           Id.print id
           print_coe c
       in
       Format.fprintf fmt "@[<hv 1>{!%a@ >> %a@ >> @[%a@] !}@]"
         (pp_list
            ~pp_left:pp_space
            ~pp_sep:(fun fmt () -> Format.fprintf fmt "@;| ")
            print_var_coercion) ctx
         print_exp exp
         print_coe res

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
         print_exp_app e1
         print_exp_simple e2
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
    Format.fprintf fmt "@[%a%a%a @,= %a@]"
      print_pat eq_lhs
      Warp.Print.(pp_list ~pp_left:pp_breakable_space print_pat) eq_params
      print_res_ty eq_ty
      print_exp eq_rhs

  and print_block fmt { b_kind; b_body; _ } =
    let pp_sep fmt () = Format.fprintf fmt ";@;@;" in
    Format.fprintf fmt "@[<v>%a@ {@[<v 2>%a@]@,}@]"
      BlockKind.print b_kind
      Warp.Print.(pp_list
                    ~pp_left:pp_breakable_space
                    ~pp_sep
                    print_eq) b_body

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
         Id.compare v1 v2
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

  let rec compare_coe_desc cd1 cd2 =
    let tag_to_int cd =
      match cd with
      | CSeq _ -> 0
      | CStream _ -> 1
      | CArr _ -> 2
      | CProd _ -> 3
      | CWarped _ -> 4
      | CInvertible _ -> 5
      | CDelay _ -> 6
    in
    match cd1, cd2 with
    | CStream c_1, CStream c_2
      ->
       compare c_1 c_2
    | CSeq (c1_1, c1_2), CSeq (c2_1, c2_2)
    | CArr (c1_1, c1_2), CArr (c2_1, c2_2)
    | CProd (c1_1, c1_2), CProd (c2_1, c2_2)
      ->
       Warp.Utils.compare_both
         (compare c1_1 c2_1)
         (fun () -> compare c1_2 c2_2)
    | CWarped (p1, c1), CWarped (p2, c2) ->
       Warp.Utils.compare_both
         (Warp.Formal.compare p1 p2)
         (fun () -> compare c1 c2)
    | CInvertible i1, CInvertible i2 ->
       Invertible.compare i1 i2
    | CDelay (p1, q1), CDelay (p2, q2) ->
       Warp.Utils.compare_both
         (Warp.Formal.compare p1 p2)
         (fun () -> Warp.Formal.compare q1 q2)
    | (CSeq _ | CStream _ | CArr _ | CProd _ | CWarped _ | CInvertible _
       | CDelay _), _ ->
       Warp.Utils.compare_int (tag_to_int cd1) (tag_to_int cd2)

  and compare_coe c1 c2 =
    Warp.Utils.compare_both
      (compare_coe_desc c1.c_desc c2.c_desc)
      (fun () -> CoeAnnot.compare c1.c_ann c2.c_ann)

  let var_compare v1 v2 =
    if v1 == v2 then 0
    else
      let tag_to_int v =
        match v with
        | VLocal _ -> 0
        | VExternal _ -> 1
      in
      match v1, v2 with
      | VLocal x1, VLocal x2 ->
         Id.compare x1 x2
      | VExternal n1, VExternal n2 ->
         Name.compare n1 n2
      | (VLocal _ | VExternal _), _ ->
        Warp.Utils.compare_int (tag_to_int v1) (tag_to_int v2)

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
        | ECons _ -> 12
        | EPair _ -> 3
        | ELet _ -> 13
        | EWhere _ -> 6
        | EConst _ -> 7
        | EBy _ -> 8
        | EAnnot _ -> 9
        | ESub _ -> 10
      in
      match ed1, ed2 with
      | EVar v1, EVar v2 ->
         var_compare v1 v2
      | ELam (p, e), ELam (p', e') ->
        Warp.Utils.compare_both
          (compare_pat p p')
          (fun () -> compare_exp e e')
      | EApp (e1, e2), EApp (e1', e2')
      | EPair (e1, e2), EPair (e1', e2')
      | ECons (e1, e2), ECons (e1', e2') ->
         Warp.Utils.compare_both
           (compare_exp e1 e1')
           (fun () -> compare_exp e2 e2')
      | ELet { block = b1; body = e1; },
        ELet { block = b2; body = e2; }
      | EWhere { body = e1; block = b1; },
        EWhere { body = e2; block = b2; } ->
        Warp.Utils.compare_both
          (compare_exp e1 e2)
          (fun () -> compare_block b1 b2)
      | EConst c, EConst c' ->
        Const.compare_const c c'
      | EBy { body = e1; dr = p1; },
        EBy { body = e2; dr = p2; } ->
        Warp.Utils.compare_both
          (Warp.Formal.compare p1 p2)
          (fun () ->
            compare_exp e1 e2)
      | EAnnot { exp = e1; kind = k1; annot = ty1; },
        EAnnot { exp = e2; kind = k2; annot = ty2; } ->
         Warp.Utils.compare_both
           (AnnotKind.compare k1 k2)
           (fun () ->
             Warp.Utils.compare_both
               (Type.compare ty1 ty2)
               (fun () ->
                 compare_exp e1 e2))
      | ESub { ctx = ctx1; exp = exp1; res = res1; },
        ESub { ctx = ctx2; exp = exp2; res = res2; } ->
         let compare_ident_coercion (v1, c1) (v2, c2) =
           Warp.Utils.compare_both
             (Id.compare v1 v2)
             (fun () -> compare_coe c1 c2)
         in
         Warp.Utils.compare_both
           (compare_exp exp1 exp2)
           (fun () ->
             Warp.Utils.compare_both
               (compare_coe res1 res2)
               (fun () ->
                 Warp.Utils.compare_list compare_ident_coercion ctx1 ctx2))
      | (EVar _ | ELam _ | EApp _ | ECons _ | EPair _ | ELet _
         | EWhere _ | EConst _ | EBy _ | EAnnot _ | ESub _), _ ->
        Warp.Utils.compare_int (tag_to_int ed1) (tag_to_int ed2)

  and compare_eq
      { eq_lhs = p1; eq_ty = ty1; eq_rhs = e1; _ }
      { eq_lhs = p2; eq_ty = ty2; eq_rhs = e2; _ } =
    Warp.Utils.compare_both
      (compare_pat p1 p2)
      (fun () ->
        Warp.Utils.compare_both
          (Warp.Utils.compare_opt Type.compare ty1 ty2)
          (fun () -> compare_exp e1 e2))

  and compare_block
    { b_kind = k1; b_body = b1; _ }
    { b_kind = k2; b_body = b2; _ } =
    Warp.Utils.compare_both
      (BlockKind.compare k1 k2)
      (fun () -> Warp.Utils.compare_list compare_eq b1 b2)

  type phr =
    {
      ph_desc : phr_desc;
      ph_loc : Loc.t;
      ph_ann : PhrAnnot.t;
    }

  and phr_desc =
    | PDef of block
    | PDecl of { id : Id.t; ty : Type.t }

  let print_phr fmt phr =
    match phr.ph_desc with
    | PDef block ->
       print_block fmt block
    | PDecl { id; ty; } ->
       Format.fprintf fmt "@[extern %a@ : %a@]"
         Id.print id
         Type.print ty

  let compare_phr phr1 phr2 =
    let tag_to_int phr =
      match phr.ph_desc with
      | PDef _ -> 0
      | PDecl _ -> 1
    in
    if phr1 == phr2 then 0
    else
      match phr1.ph_desc, phr2.ph_desc with
      | PDef b1, PDef b2 ->
         compare_block b1 b2
      | PDecl { id = id1; ty = ty1; },
        PDecl { id = id2; ty = ty2; } ->
         Warp.Utils.compare_both
           (Id.compare id1 id2)
           (fun () -> Type.compare ty1 ty2)
      | (PDef _ | PDecl _), _ ->
         Warp.Utils.compare_int (tag_to_int phr1) (tag_to_int phr2)

  type file =
      {
        f_name : string;
        f_phrases : phr list;
        f_loc : Loc.t;
        f_annot : FileAnnot.t;
      }

  let print_file fmt { f_phrases; _ } =
    Warp.Print.pp_list
      ~pp_left:(fun fmt () -> Format.fprintf fmt "@\n")
      ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n@\n")
      ~pp_right:(fun fmt () -> Format.fprintf fmt "@\n")
      print_phr
      fmt
      f_phrases;
    ()

  let compare_file
      { f_name = n1; f_phrases = body1; _ }
      { f_name = n2; f_phrases = body2; _ } =
    Warp.Utils.compare_both
      (Warp.Utils.compare_string n1 n2)
      (fun () -> Warp.Utils.compare_list compare_phr body1 body2)
end
