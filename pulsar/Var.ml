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

module Kind = Ident.Make(Wdr.Ext.String)
module Ty = Ident.Make(Wdr.Ext.String)
module DTy = Ident.Make(Wdr.Ext.String)
module Term = Ident.Make(Wdr.Ext.String)

type kid = Kind.t
type tyid = Ty.t
type dtyid = DTy.t
type id = Term.t
type cid = C of id
