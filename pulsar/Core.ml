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

module Id = Ident.Make(Wdr.Ext.String)

type pat =
  {
    p_desc : pat_desc;
    p_type : CoreTypes.ty;
    p_loc : Loc.t;
  }

and pat_desc =
  | Pvar of Id.t
  | Ptuple of pat list

type exp =
  {
    e_desc : exp_desc;
    e_type : CoreTypes.ty;
    e_loc : Loc.t;
  }

and exp_desc =
  | Evar of Id.t
  | Efun of pat list * exp
  | Eapp of exp * exp
  | Eshut of CoreWarp.t * exp
  | Eopen of CoreWarp.t * exp
