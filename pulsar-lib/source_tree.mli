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

module AnnotKind :
sig
  type t =
    | Typing
    | Subtyping

  include Warp.Utils.PrintableOrderedType with type t := t
end

module BlockKind :
sig
  type t =
    | Seq
    | Par
    | Rec

  val default : t

  include Warp.Utils.PrintableOrderedType with type t := t
end

module type Info =
sig
  module Id : Warp.Utils.PrintableOrderedType

  module PatAnnot : Warp.Utils.PrintableOrderedType
  module CoeAnnot : Warp.Utils.PrintableOrderedType
  module ExpAnnot : Warp.Utils.PrintableOrderedType
  module EquAnnot : Warp.Utils.PrintableOrderedType
  module PhrAnnot : Warp.Utils.PrintableOrderedType
  module FileAnnot : Warp.Utils.PrintableOrderedType
end

module type Tree =
sig
  include Info

  (** Patterns *)

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

  (** Coercions *)

  type coe =
    {
      c_desc : coe_desc;
      c_loc : Loc.t;
      c_ann : CoeAnnot.t;
    }

  and coe_desc =
    | CSeq of coe * coe
    | CArr of coe * coe
    | CProd of coe * coe
    | CWarped of Warp.Formal.t * coe
    | CInvertible of Invertible.t
    | CDelay of Warp.Formal.t * Warp.Formal.t

  (** Expressions, the main syntactic category *)
  type exp =
      {
        e_desc : exp_desc;
        e_loc : Loc.t;
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
    | EBy of { body : exp; dr : Warp.Formal.t; }
    | EAnnot of { exp : exp; kind : AnnotKind.t; annot : Type.t; }
    | ESub of { ctx : (Id.t * coe) list; exp : exp; res : coe; }

  (** Equations "lhs (: ty) = rhs" *)
  and eq =
      {
        eq_lhs : pat;
        eq_params : pat list;
        eq_ty : Type.t option;
        eq_rhs : exp;
        eq_loc : Loc.t;
        eq_ann : EquAnnot.t;
      }

  (** Equation blocks kind { eq1; ...; eqN } *)
  and block =
    {
      b_kind : BlockKind.t;
      b_body : eq list;
      b_loc : Loc.t;
    }

  (** Pretty-print a pattern *)
  val print_pat : Format.formatter -> pat -> unit

  (** Pretty-print a coercion *)
  val print_coe : Format.formatter -> coe -> unit

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
      ph_loc : Loc.t;
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
        f_loc : Loc.t;
        f_annot : FileAnnot.t;
      }

  (** Pretty-print a file *)
  val print_file : Format.formatter -> file -> unit

  (** Comparison function for files a la [Pervasives.compare] *)
  val compare_file : file -> file -> int
end

module Make (I : Info) : Tree with type Id.t = I.Id.t
                               and type PatAnnot.t = I.PatAnnot.t
                               and type CoeAnnot.t = I.CoeAnnot.t
                               and type ExpAnnot.t = I.ExpAnnot.t
                               and type EquAnnot.t = I.EquAnnot.t
                               and type PhrAnnot.t = I.PhrAnnot.t
                               and type FileAnnot.t = I.FileAnnot.t
