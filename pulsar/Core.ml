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
  | Ecase of exp * branch list
  | Elet of pat * exp * exp

and branch =
  {
    b_pat : pat;
    b_body : exp;
    b_loc : Loc.t;
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

type ty_decl =
  {
    tydecl_def : Var.tyid;
    tydecl_kind : CoreTypes.kind;
    tydecl_loc : Loc.t;
  }

type val_decl =
  {
    valdecl_id : Var.id;
    valdecl_type : CoreTypes.ty;
    valdecl_loc : Loc.t;
  }

type val_def =
  {
    valdef_id : Var.id;
    valdef_type : CoreTypes.ty option;
    valdef_body : exp;
    valdef_loc : Loc.t;
  }

type modsig =
  {
    m_name : string;
  }
