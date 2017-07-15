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
    | By of { body : exp; dr : Warp_type.t; }
    | Annot of { exp : exp; kind : annot_kind; annot : Types.ty; }
    | Sub of (id * Coercions.t) list * exp * Coercions.t

  (** Definitions "x : ty = e" *)
  and def =
      {
        is_rec : bool;
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

module Make (I : Info) : Tree with type id = I.id and type ann = I.ann
