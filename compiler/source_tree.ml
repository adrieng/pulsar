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

  type exp =
      {
        desc : exp_desc;
        loc : Loc.loc;
        ann : ann;
      }

  and exp_desc =
    | Var of id
    | Lam of id * exp
    | App of exp * exp
    | Pair of exp * exp
    | Fst of exp
    | Snd of exp
    | Where of { body : exp; is_rec : bool; defs : def list; }
    | Const of Const.const
    | Scale of { body : exp; dr : Warp_type.t; locals : decl list; }
    | Annot of { exp : exp; kind : annot_kind; annot : Types.ty; }
    | Sub of (id * Coercions.t) list * exp * Coercions.t

  and def =
      {
        lhs : id;
        tydf : Types.ty;
        rhs : exp;
        locdf : Loc.loc;
      }

  and decl =
      {
        name : id;
        tydl : Types.ty;
        locdl : Loc.loc;
      }

  val print_exp : Format.formatter -> exp -> unit

  val print_def : Format.formatter -> def -> unit

  val print_decl : Format.formatter -> decl -> unit

  val compare_exp : exp -> exp -> int

  val compare_def : def -> def -> int

  val compare_decl : decl -> decl -> int

  type phr =
    | Def of def

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

  type exp =
      {
        desc : exp_desc;
        loc : Loc.loc;
        ann : ann;
      }

  and exp_desc =
    | Var of I.id
    | Lam of id * exp
    | App of exp * exp
    | Pair of exp * exp
    | Fst of exp
    | Snd of exp
    | Where of { body : exp; is_rec : bool; defs : def list; }
    | Const of Const.const
    | Scale of { body : exp; dr : Warp_type.t; locals : decl list; }
    | Annot of { exp : exp; kind : annot_kind; annot : Types.ty; }
    | Sub of (id * Coercions.t) list * exp * Coercions.t

  and def =
      {
        lhs : id;
        tydf : Types.ty;
        rhs : exp;
        locdf : Loc.loc;
      }

  and decl =
      {
        name : id;
        tydl : Types.ty;
        locdl : Loc.loc;
      }

  let rec print_exp fmt e =
    let open Warp.Print in
    match e.desc with
    | Var x ->
      print_id fmt x
    | Lam (x, e) ->
      Format.fprintf fmt "@[%a%a.%a@]"
        Pp.print_lam ()
        print_id x
        print_exp e
    | App (e1, e2) ->
      Format.fprintf fmt "@[<v>%a@ %a@]"
        print_exp e1
        print_exp e2
    | Pair (e1, e2) ->
      Format.fprintf fmt "(@[<v>%a,@ %a@])"
        print_exp e1
        print_exp e2
    | Where { body; is_rec; defs; } ->
      Format.fprintf fmt "@[%a where%s@ {@[<v 2>%a@]}@]"
        print_exp body
        (if is_rec then " rec" else "")
        (pp_list ~pp_sep:pp_semicolon print_def) defs
    | Fst e ->
      Format.fprintf fmt "@[fst@ %a@]"
        print_exp e
    | Snd e ->
      Format.fprintf fmt "@[snd@ %a@]"
        print_exp e
    | Const c ->
      Const.print_const fmt c
    | Scale { body; dr; locals; } ->
      Format.fprintf fmt "@[scale %a@ by %a@ with %a@]"
        print_exp body
        Warp_type.print dr
        (pp_list ~pp_sep:pp_semicolon print_decl) locals
    | Annot { exp = e; kind = a; annot = ty; } ->
      Format.fprintf fmt "@[%a@ %a %a@]"
        print_exp e
        print_annot_kind a
        Types.print_ty ty
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

  and print_def fmt { lhs; tydf; rhs; } =
    Format.fprintf fmt "@[%a @[@,: %a @,= %a@]@]"
      print_id lhs
      Types.print_ty tydf
      print_exp rhs

  and print_decl fmt { name; tydl; } =
    Format.fprintf fmt "@[%a : %a@]"
      print_id name
      Types.print_ty tydl

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
        | Scale _ -> 8
        | Annot _ -> 9
        | Sub _ -> 10
      in
      match ed1, ed2 with
      | Var v1, Var v2 ->
        compare_id v1 v2
      | Lam (x, e), Lam (x', e') ->
        Warp.Utils.compare_both
          (compare_id x x')
          (fun () -> compare_exp e e')
      | App (e1, e2), App (e1', e2')
      | Pair (e1, e2), Pair (e1', e2') ->
         Warp.Utils.compare_both
           (compare_exp e1 e1')
           (fun () -> compare_exp e2 e2')
      | Fst e, Fst e' | Snd e, Snd e' ->
        compare_exp e e'
      | Where { body = e1; is_rec = r1; defs = b1; },
        Where { body = e2; is_rec = r2; defs = b2; } ->
        Warp.Utils.compare_both
          (Warp.Utils.compare_bool r1 r2)
          (fun () ->
            Warp.Utils.compare_both
              (compare_exp e1 e2)
              (fun () -> Warp.Utils.compare_list compare_def b1 b2))
      | Const c, Const c' ->
        Const.compare_const c c'
      | Scale { body = e; dr = p;  locals = l; },
        Scale { body = e'; dr = p'; locals = l'; } ->
        Warp.Utils.compare_both
          (Warp_type.compare p p')
          (fun () ->
            Warp.Utils.compare_both
              (compare_exp e e')
              (fun () -> Warp.Utils.compare_list compare_decl l l'))
      | Annot { exp = e1; kind = k1; annot = ty1; },
        Annot { exp = e2; kind = k2; annot = ty2; } ->
         Warp.Utils.compare_both
           (compare_annot_kind k1 k2)
           (fun () ->
             Warp.Utils.compare_both
               (Types.compare_ty ty1 ty2)
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
          Scale _ | Annot _ | Sub _), _ ->
        Warp.Utils.compare_int (tag_to_int ed1) (tag_to_int ed2)

  and compare_def
      { lhs = x1; tydf = ty1; rhs = e1; }
  { lhs = x2; tydf = ty2; rhs = e2; } =
    Warp.Utils.compare_both
      (compare_id x1 x2)
      (fun () ->
        Warp.Utils.compare_both
          (Types.compare_ty ty1 ty2)
          (fun () -> compare_exp e1 e2))

  and compare_decl { name = x1; tydl = ty1; } { name = x2; tydl = ty2; } =
    Warp.Utils.compare_both
      (compare_id x1 x2)
      (fun () -> Types.compare_ty ty1 ty2)

  type phr =
    | Def of def

  let print_phr fmt phr =
    match phr with
    | Def { lhs; tydf; rhs; } ->
      Format.fprintf fmt "@[let %a @[: %a@ = @[%a@]@]@]"
        print_id lhs
        Types.print_ty tydf
        print_exp rhs

  let compare_phr phr1 phr2 =
    if phr1 == phr2 then 0
    else
      match phr1, phr2 with
      | Def d1, Def d2 ->
        compare_def d1 d2

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
