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
      desc : pat_desc;
      loc : Loc.loc;
      ann : ann;
    }

  and pat_desc =
    | Var of id
    | Pair of pat * pat
    | Cons of pat * pat
    | Annot of pat * Type.t

  type exp =
      {
        desc : exp_desc;
        loc : Loc.loc;
        ann : ann;
      }

  and exp_desc =
    | Var of id
    | Lam of pat * exp
    | App of exp * exp
    | Pair of exp * exp
    | Fst of exp
    | Snd of exp
    | Where of { body : exp; is_rec : bool; eqs : eq list; }
    | Const of Const.const
    | By of { body : exp; dr : Warp_type.t; }
    | Annot of { exp : exp; kind : annot_kind; annot : Type.t; }
    | Sub of (id * Coercions.t) list * exp * Coercions.t

  and eq =
      {
        lhs : pat;
        params : pat list;
        res_ty : Type.t option;
        rhs : exp;
        locdf : Loc.loc;
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
        name : string;
        phrases : phr list;
      }

  val print_file : Format.formatter -> file -> unit

  val compare_file : file -> file -> int
end

module Make = functor (I : Info) ->
struct
  include I

  type pat =
    {
      desc : pat_desc;
      loc : Loc.loc;
      ann : ann;
    }

  and pat_desc =
    | Var of id
    | Pair of pat * pat
    | Cons of pat * pat
    | Annot of pat * Type.t

  type exp =
      {
        desc : exp_desc;
        loc : Loc.loc;
        ann : ann;
      }

  and exp_desc =
    | Var of I.id
    | Lam of pat * exp
    | App of exp * exp
    | Pair of exp * exp
    | Fst of exp
    | Snd of exp
    | Where of { body : exp; is_rec : bool; eqs : eq list; }
    | Const of Const.const
    | By of { body : exp; dr : Warp_type.t; }
    | Annot of { exp : exp; kind : annot_kind; annot : Type.t; }
    | Sub of (id * Coercions.t) list * exp * Coercions.t

  and eq =
      {
        lhs : pat;
        params : pat list;
        res_ty : Type.t option;
        rhs : exp;
        locdf : Loc.loc;
      }

  let rec print_pat fmt (p : pat) =
    match p.desc with
    | Var x ->
       print_id fmt x
    | Pair (p1, p2) ->
       Format.fprintf fmt "(@[%a,@;%a@])"
         print_pat p1
         print_pat p2
    | Cons (p1, p2) ->
       Format.fprintf fmt "(@[%a ::@;%a@])"
         print_pat p1
         print_pat p2
    | Annot (p, ty) ->
       Format.fprintf fmt "(@[%a :@;%a@])"
         print_pat p
         Type.print ty

  let rec print_exp_prio prio fmt e =
    let open Warp.Print in
    match e.desc with
    | Var x ->
      print_id fmt x

    | Lam (p, e) ->
      Format.fprintf fmt "@[<hov 2>%a %a %a@ %a@]"
        Pp.print_lam ()
        print_pat p
        Pp.print_warr ()
        print_exp e

    | App ({ desc = App ({ desc = Const Const.Op op; _ }, e1); _ }, e2) ->
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

    | App (e1, e2) ->
      Format.fprintf fmt "@[<hov 2>%a %a@]"
        print_exp e1
        print_exp_app e2

    | Pair (e1, e2) ->
      Format.fprintf fmt "(@[<v>%a,@ %a@])"
        print_exp e1
        print_exp e2

    | Where { body; is_rec; eqs; } ->
      Format.fprintf fmt "@[%a where%s@ {@[<v 2>%a@]}@]"
        print_exp body
        (if is_rec then " rec" else "")
        (pp_list ~pp_sep:pp_semicolon print_eq) eqs

    | Fst e ->
      Format.fprintf fmt "@[fst@ %a@]"
        print_exp e

    | Snd e ->
      Format.fprintf fmt "@[snd@ %a@]"
        print_exp e

    | Const c ->
      Const.print_const fmt c

    | By { body; dr; } ->
      Format.fprintf fmt "@[%a@ by %a@]"
        print_exp_simple body
        Warp_type.print dr

    | Annot { exp = e; kind = a; annot = ty; } ->
      Format.fprintf fmt "@[%a@ %a %a@]"
        print_exp e
        print_annot_kind a
        Type.print ty

    | Sub (ctx_c, e, c) ->
       let print_ident_coercion fmt (id, c) =
         Format.fprintf fmt "(%a <<@ %a)"
           print_id id
           Coercions.print c
       in
       Format.fprintf fmt "@[<b>{!@[<2>@[%a@]@ >> %a@ >> %a @]!}@]"
         (pp_list
            ~pp_left:pp_breakable_space
            ~pp_sep:pp_comma
            print_ident_coercion) ctx_c
         print_exp e
         Coercions.print c

  and print_exp_simple fmt e =
    match e.desc with
    | Var _ | Const (Const.Lit _) ->
       print_exp fmt e
    | _ ->
       Format.fprintf fmt "(%a)"
         print_exp e

  and print_exp_app fmt e =
    match e.desc with
    | App (e1, e2) ->
       Format.fprintf fmt "%a@ %a"
         print_exp e1
         print_exp_app e2
    | _ ->
       print_exp fmt e

  and print_exp fmt e =
    print_exp_prio 0 fmt e

  and print_eq fmt { lhs; params; res_ty; rhs; _ } =
    let print_res_ty =
      Warp.Print.pp_opt
        ~pp_left:Warp.Print.pp_breakable_space
        (fun fmt ty -> Format.fprintf fmt ": %a" Type.print ty)
    in
    Format.fprintf fmt "@[%a%a%a @,= %a@]@]"
      print_pat lhs
      Warp.Print.(pp_list ~pp_left:pp_breakable_space print_pat) params
      print_res_ty res_ty
      print_exp rhs

  let rec compare_pat (p1 : pat) (p2 : pat) =
    if p1 == p2 then 0
    else
      let tag_to_int (pd : pat_desc) =
        match pd with
        | Var _ -> 0
        | Pair _ -> 1
        | Cons _ -> 2
        | Annot _ -> 3
      in
      match p1.desc, p2.desc with
      | Var v1, Var v2 ->
         compare_id v1 v2
      | Pair (p11, p12), Pair (p21, p22)
      | Cons (p11, p12), Cons (p21, p22) ->
         Warp.Utils.compare_both
           (compare_pat p11 p21)
           (fun () -> compare_pat p12 p22)
      | Annot (p1, ty1), Annot (p2, ty2) ->
         Warp.Utils.compare_both
           (compare_pat p1 p2)
           (fun () -> Type.compare ty1 ty2)
      | (Var _ | Pair _ | Cons _ | Annot _), _ ->
         Warp.Utils.compare_int (tag_to_int p1.desc) (tag_to_int p2.desc)

  let rec compare_exp e1 e2 =
    if e1 == e2 then 0 else compare_exp_desc e1.desc e2.desc

  and compare_exp_desc ed1 ed2 =
    if ed1 == ed2 then 0
    else
      let tag_to_int ed =
        match ed with
        | Var _ -> 0
        | Lam _ -> 1
        | App _ -> 2
        | Pair _ -> 3
        | Fst _ -> 4
        | Snd _ -> 5
        | Where _ -> 6
        | Const _ -> 7
        | By _ -> 8
        | Annot _ -> 9
        | Sub _ -> 10
      in
      match ed1, ed2 with
      | Var v1, Var v2 ->
        compare_id v1 v2
      | Lam (p, e), Lam (p', e') ->
        Warp.Utils.compare_both
          (compare_pat p p')
          (fun () -> compare_exp e e')
      | App (e1, e2), App (e1', e2')
      | Pair (e1, e2), Pair (e1', e2') ->
         Warp.Utils.compare_both
           (compare_exp e1 e1')
           (fun () -> compare_exp e2 e2')
      | Fst e, Fst e' | Snd e, Snd e' ->
        compare_exp e e'
      | Where { body = e1; is_rec = r1; eqs = eqs1; },
        Where { body = e2; is_rec = r2; eqs = eqs2; } ->
        Warp.Utils.compare_both
          (Warp.Utils.compare_bool r1 r2)
          (fun () ->
            Warp.Utils.compare_both
              (compare_exp e1 e2)
              (fun () -> Warp.Utils.compare_list compare_eq eqs1 eqs2))
      | Const c, Const c' ->
        Const.compare_const c c'
      | By { body = e1; dr = p1; },
        By { body = e2; dr = p2; } ->
        Warp.Utils.compare_both
          (Warp_type.compare p1 p2)
          (fun () ->
            compare_exp e1 e2)
      | Annot { exp = e1; kind = k1; annot = ty1; },
        Annot { exp = e2; kind = k2; annot = ty2; } ->
         Warp.Utils.compare_both
           (compare_annot_kind k1 k2)
           (fun () ->
             Warp.Utils.compare_both
               (Type.compare ty1 ty2)
               (fun () ->
                 compare_exp e1 e2))
      | Sub (ctx_c1, e1, c1), Sub (ctx_c2, e2, c2) ->
         let compare_ident_coercion (v1, c1) (v2, c2) =
           Warp.Utils.compare_both
             (compare_id v1 v2)
             (fun () -> Coercions.compare c1 c2)
         in
         Warp.Utils.compare_both
           (compare_exp e1 e2)
           (fun () ->
             Warp.Utils.compare_both
               (Coercions.compare c1 c2)
               (fun () ->
                 Warp.Utils.compare_list compare_ident_coercion ctx_c1 ctx_c2))
      | (Var _ | Lam _ | App _ | Pair _ | Fst _ | Snd _ | Where _ | Const _ |
          By _ | Annot _ | Sub _), _ ->
        Warp.Utils.compare_int (tag_to_int ed1) (tag_to_int ed2)

  and compare_eq
      { lhs = p1; res_ty = ty1; rhs = e1; }
      { lhs = p2; res_ty = ty2; rhs = e2; } =
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
        name : string;
        phrases : phr list;
      }

  let print_file fmt { name; phrases; } =
    Format.fprintf fmt "(* File \"%s\" *)@\n" name;
    List.iter (fun phr -> Format.fprintf fmt "@\n%a@\n" print_phr phr) phrases

  let compare_file
      { name = n1; phrases = body1; }
      { name = n2; phrases = body2; } =
    Warp.Utils.compare_both
      (Warp.Utils.compare_string n1 n2)
      (fun () -> Warp.Utils.compare_list compare_phr body1 body2)
end
