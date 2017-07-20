type annot_kind =
  | Typing
  | Subtyping

val print_annot_kind : Format.formatter -> annot_kind -> unit

val compare_annot_kind : annot_kind -> annot_kind -> int

module type Info =
sig
  module Id : Warp.Utils.PrintableOrderedType

  module PatAnnot : Warp.Utils.PrintableOrderedType
  module ExpAnnot : Warp.Utils.PrintableOrderedType
  module EquAnnot : Warp.Utils.PrintableOrderedType
  module PhrAnnot : Warp.Utils.PrintableOrderedType
end

module type Tree =
sig
  include Info

  (** Patterns *)

  type pat =
    {
      p_desc : pat_desc;
      p_loc : Loc.loc;
      p_ann : PatAnnot.t;
    }

  and pat_desc =
    | PVar of Id.t
    | PPair of pat * pat
    | PCons of pat * pat
    | PAnnot of pat * Type.t

  (** Expressions, the main syntactic category *)
  type exp =
      {
        e_desc : exp_desc;
        e_loc : Loc.loc;
        e_ann : ExpAnnot.t;
      }

  (** Expression bodies *)
  and exp_desc =
    | EVar of Id.t
    | EExternal of Name.t
    | ELam of pat * exp
    | EApp of exp * exp
    | ECons of exp * exp
    | EPair of exp * exp
    | EFst of exp
    | ESnd of exp
    | ELet of { block : block; body : exp; }
    | EWhere of { body : exp; block : block; }
    | EConst of Const.const
    | EBy of { body : exp; dr : Warp_type.t; }
    | EAnnot of { exp : exp; kind : annot_kind; annot : Type.t; }
    | ESub of { ctx : (Id.t * Coercion.t) list; exp : exp; res : Coercion.t; }

  (** Equations "lhs (: ty) = rhs" *)
  and eq =
      {
        eq_lhs : pat;
        eq_params : pat list;
        eq_ty : Type.t option;
        eq_rhs : exp;
        eq_loc : Loc.loc;
        eq_ann : EquAnnot.t;
      }

  (** Equation blocks rec? { eq1; ...; eqN } *)
  and block =
    {
      b_rec : bool;
      b_body : eq list;
      b_loc : Loc.loc;
    }

  (** Pretty-print a pattern *)
  val print_pat : Format.formatter -> pat -> unit

  (** Pretty-print an expression *)
  val print_exp : Format.formatter -> exp -> unit

  (** Pretty-print an equation *)
  val print_eq : Format.formatter -> eq -> unit

  (** Pretty-print a block *)
  val print_block : Format.formatter -> block -> unit

  (** Comparison function for expressions a la [Pervasives.compare]. *)
  val compare_exp : exp -> exp -> int

  (** Comparison function for equations a la [Pervasives.compare]. *)
  val compare_eq : eq -> eq -> int

  (** Phrases, that is, top-level statements *)
  type phr =
    {
      ph_desc : phr_desc;
      ph_loc : Loc.loc;
      ph_ann : PhrAnnot.t;
    }

  and phr_desc =
    | PDef of block
    | PDecl of { id : Id.t; ty : Type.t }

  (** Pretty-print a phrase *)
  val print_phr : Format.formatter -> phr -> unit

  (** Comparison function for phrases a la [Pervasives.compare] *)
  val compare_phr : phr -> phr -> int

  (** Complete files *)
  type file =
      {
        f_name : string;
        f_phrases : phr list;
      }

  (** Pretty-print a file *)
  val print_file : Format.formatter -> file -> unit

  (** Comparison function for files a la [Pervasives.compare] *)
  val compare_file : file -> file -> int
end

module Make (I : Info) : Tree with type Id.t = I.Id.t
                               and type PatAnnot.t = I.PatAnnot.t
                               and type ExpAnnot.t = I.ExpAnnot.t
                               and type EquAnnot.t = I.EquAnnot.t
                               and type PhrAnnot.t = I.PhrAnnot.t
