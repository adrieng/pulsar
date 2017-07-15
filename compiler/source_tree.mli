type annot_kind =
  | Typing
  | Subtyping

val print_annot_kind : Format.formatter -> annot_kind -> unit

val compare_annot_kind : annot_kind -> annot_kind -> int

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

  (** Patterns *)

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
    | Lam of pat * exp
    | App of exp * exp
    | Pair of exp * exp
    | Fst of exp
    | Snd of exp
    | Where of { body : exp; is_rec : bool; defs : def list; }
    | Const of Const.const
    | By of { body : exp; dr : Warp_type.t; }
    | Annot of { exp : exp; kind : annot_kind; annot : Type.t; }
    | Sub of (id * Coercions.t) list * exp * Coercions.t

  (** Definitions "p = e" *)
  and def =
      {
        lhs : pat;
        res_ty : Type.t option;
        rhs : exp;
        locdf : Loc.loc;
      }

  (** Pretty-print an expression *)
  val print_exp : Format.formatter -> exp -> unit

  (** Pretty-print a definition *)
  val print_def : Format.formatter -> def -> unit

  (** Comparison function for expressions a la [Pervasives.compare]. *)
  val compare_exp : exp -> exp -> int

  (** Comparison function for definitions a la [Pervasives.compare]. *)
  val compare_def : def -> def -> int

  (** Phrases, that is, top-level statements *)
  type phr =
    | Def of { is_rec : bool; def : def }

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

module Make (I : Info) : Tree with type id = I.id and type ann = I.ann
