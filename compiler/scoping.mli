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

type scoping_error =
  | Unbound_identifier of string * Loc.loc
  | Duplicate_identifier of string * Loc.loc

exception Scoping_error of scoping_error

val print_scoping_error : scoping_error Warp.Print.printer

val pass : (Raw_tree.T.file -> Scoped_tree.T.file) Pass.t
