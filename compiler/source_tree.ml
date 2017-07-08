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

  (** Expressions, the main syntactic category *)
  type exp =
      {
        desc : exp_desc;
        loc : Loc.loc;
        ann : ann;
      }

  (** Expression bodies *)
  and exp_desc =
    | Var of id
    | Lam of id * exp
    | App of exp * exp
    | Pair of exp * exp
    | Fst of exp
    | Snd of exp
    | Where of { body : exp; is_rec : bool; defs : def list; }
    | Const of Const.const
    | BoxApp of exp * exp
    | Shift of exp * Clock_type.t * Types.ty
    | Scale of { body : exp; dr : Clock_type.t; locals : decl list; }
    | Annot of exp * Types.ty

  (** Definitions "x : ty = e" *)
  and def =
      {
        lhs : id;
        tydf : Types.ty;
        rhs : exp;
        locdf : Loc.loc;
      }

  (** Declarations "x : ty" *)
  and decl =
      {
        name : id;
        tydl : Types.ty;
        locdl : Loc.loc;
      }

  (** Pretty-print an expression *)
  val print_exp : Format.formatter -> exp -> unit

  (** Pretty-print a definition *)
  val print_def : Format.formatter -> def -> unit

  (** Pretty-print a declaration *)
  val print_decl : Format.formatter -> decl -> unit

  (** Comparison function for expressions a la [Pervasives.compare]. *)
  val compare_exp : exp -> exp -> int

  (** Comparison function for definitions a la [Pervasives.compare]. *)
  val compare_def : def -> def -> int

  (** Comparison function for declarations a la [Pervasives.compare]. *)
  val compare_decl : decl -> decl -> int

  (** Phrases, that is, top-level statements *)
  type phr =
    | Def of def

  (** Pretty-print a phrase *)
  val print_phr : Format.formatter -> phr -> unit

  (** Comparison function for phrases a la [Pervasives.compare] *)
  val compare_phr : phr -> phr -> int

  (** Complete files *)
  type file =
      {
        name : string;
        phrases : phr list;
      }

  (** Pretty-print a file *)
  val print_file : Format.formatter -> file -> unit

  (** Comparison function for files a la [Pervasives.compare] *)
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
    | BoxApp of exp * exp
    | Shift of exp * Clock_type.t * Types.ty
    | Scale of { body : exp; dr : Clock_type.t; locals : decl list; }
    | Annot of exp * Types.ty

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
        (Clock.Utils.print_list_sep_r print_def ";") defs
    | Fst e ->
      Format.fprintf fmt "@[fst@ %a@]"
        print_exp e
    | Snd e ->
      Format.fprintf fmt "@[snd@ %a@]"
        print_exp e
    | Const c ->
      Const.print_const fmt c
    | BoxApp (e1, e2) ->
      Format.fprintf fmt "@[<v>%a %a@ %a@]"
        print_exp e1
        Pp.print_bapp ()
        print_exp e2
    | Shift (e, ck, ty) ->
      Format.fprintf fmt "@[shift %a@ to %a@]"
        print_exp e
        Types.print_ty (Types.Box (ck, ty))
    | Scale { body; dr; locals; } ->
      Format.fprintf fmt "@[scale %a@ by %a@ with %a@]"
        print_exp body
        Clock_type.print dr
        (Clock.Utils.print_list_sep_r print_decl ";") locals
    | Annot (e, ty) ->
      Format.fprintf fmt "@[%a@ : %a@]"
        print_exp e
        Types.print_ty ty

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
        | BoxApp _ -> 8
        | Shift _ -> 9
        | Scale _ -> 10
        | Annot _ -> 11
      in
      match ed1, ed2 with
      | Var v1, Var v2 ->
        Clock.Utils.compare_string v1 v2
      | Lam (x, e), Lam (x', e') ->
        Clock.Utils.compare_both
          (Clock.Utils.compare_string x x')
          (fun () -> compare_exp e e')
      | App (e1, e2), App (e1', e2')
      | Pair (e1, e2), Pair (e1', e2')
      | BoxApp (e1, e2), BoxApp (e1', e2') ->
        Clock.Utils.compare_both
          (compare_exp e1 e1')
          (fun () -> compare_exp e2 e2')
      | Fst e, Fst e' | Snd e, Snd e' ->
        compare_exp e e'
      | Where { body = e1; is_rec = r1; defs = b1; },
        Where { body = e2; is_rec = r2; defs = b2; } ->
        Clock.Utils.compare_both
          (Clock.Utils.compare_bool r1 r2)
          (fun () ->
            Clock.Utils.compare_both
              (compare_exp e1 e2)
              (fun () -> Clock.Utils.compare_list compare_def b1 b2))
      | Const c, Const c' ->
        Const.compare_const c c'
      | Shift (e, p, ty), Shift (e', p', ty') ->
        Clock.Utils.compare_both
          (Clock_type.compare p p')
          (fun () ->
            Clock.Utils.compare_both
              (Types.compare_ty ty ty')
              (fun () -> compare_exp e e'))
      | Scale { body = e; dr = p;  locals = l; },
        Scale { body = e'; dr = p'; locals = l'; } ->
        Clock.Utils.compare_both
          (Clock_type.compare p p')
          (fun () ->
            Clock.Utils.compare_both
              (compare_exp e e')
              (fun () -> Clock.Utils.compare_list compare_decl l l'))
      | Annot (e, ty), Annot (e', ty') ->
        Clock.Utils.compare_both
          (Types.compare_ty ty ty')
          (fun () -> compare_exp e e')
      | (Var _ | Lam _ | App _ | Pair _ | Fst _ | Snd _ | Where _ | Const _ |
          BoxApp _ | Shift _ | Scale _ | Annot _), _ ->
        Clock.Utils.compare_int (tag_to_int ed1) (tag_to_int ed2)

  and compare_def
      { lhs = x1; tydf = ty1; rhs = e1; }
  { lhs = x2; tydf = ty2; rhs = e2; } =
    Clock.Utils.compare_both
      (Clock.Utils.compare_string x1 x2)
      (fun () ->
        Clock.Utils.compare_both
          (Types.compare_ty ty1 ty2)
          (fun () -> compare_exp e1 e2))

  and compare_decl { name = x1; tydl = ty1; } { name = x2; tydl = ty2; } =
    Clock.Utils.compare_both
      (Clock.Utils.compare_string x1 x2)
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
    Clock.Utils.compare_both
      (Clock.Utils.compare_string n1 n2)
      (fun () -> Clock.Utils.compare_list compare_phr body1 body2)
end
