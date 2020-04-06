(* This file is part of Pulsar, a temporal functional language.
 * Copyright (C) 2017-2020 Adrien Guatto
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

type ty = CoreTypes.ty

type pat =
  {
    p_desc : pat_desc;
    p_type : ty;
    p_loc : Loc.t;
  }

and pat_desc =
  | Pid of Var.id
  | Ptuple of pat list
  | Pshut of CoreWarp.t * pat
  | Pconstr of Var.cid * pat list

type exp =
  {
    e_desc : exp_desc;
    e_type : ty;
    e_loc : Loc.t;
  }

and exp_desc =
  | Evar of Var.id
  | Efun of pat list * exp
  | Eapp of exp * exp
  | Etuple of exp list
  | Eshut of CoreWarp.t * exp
  | Eopen of CoreWarp.t * exp
  | Ecase of exp * branches
  | Elet of eqs * exp
  | Ewhere of exp * eqs

and branches =
  branch list

and branch =
  {
    b_pat : pat;
    b_body : exp;
    b_loc : Loc.t;
  }

and eq =
  {
    eq_lhs : pat;
    eq_rhs : exp;
    eq_loc : Loc.t;
  }

and eqs =
  {
    eqs_binding : [`Seq | `Rec | `Par];
    eqs_body : eq list;
    eqs_loc : Loc.t;
  }

type kind_def =
  {
    kdef_id : Var.kid;
    kdef_body : CoreTypes.kind;
    kdef_loc : Loc.t;
  }

type ty_def =
  {
    tydef_id : Var.tyid;
    tydef_kind : CoreTypes.kind option;
    tydef_body : CoreTypes.ty;
    tydef_loc : Loc.t;
  }

type ty_dec =
  {
    tydec_def : Var.tyid;
    tydec_kind : CoreTypes.kind;
    tydec_loc : Loc.t;
  }

type val_dec =
  {
    valdec_id : Var.id;
    valdec_type : CoreTypes.ty;
    valdec_loc : Loc.t;
  }

type val_def =
  {
    valdef_id : Var.id;
    valdef_type : CoreTypes.ty option;
    valdef_body : exp;
    valdef_loc : Loc.t;
  }

type modsig_phrase =
  [
  | `KDef of kind_def
  | `TDec of ty_dec
  | `TDef of ty_def
  | `VDec of val_dec
  ]

type modstruct_phrase =
  [ modsig_phrase | `VDef of val_def ]

type modsig =
  {
    m_name : string;
    m_contents : modsig_phrase list;
  }

type modstruct =
  {
    m_name : string;
    m_contents : modstruct_phrase list;
  }
