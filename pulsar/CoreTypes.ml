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

type warp = CoreWarp.t

type kind =
  {
    k_desc : kind_desc;
    k_loc : Loc.t;
  }

and kind_desc =
  | Kvar of Var.kid
  | Kforall of Var.kid option * kind * kind
  | Kapp of kind * kind
  | Kmod of warp * kind

type ty =
  {
    ty_desc : ty_desc;
    ty_kind : kind;
    ty_loc : Loc.t;
  }

and ty_desc =
  | Tvar of Var.tyid
  | Tdata of Var.dtyid
  | Twarp of CoreWarp.t
  | Tforall of Var.tyid * kind * ty
  | Tapp of ty * ty
  | Tshut of warp * ty
  | Topen of warp * ty
