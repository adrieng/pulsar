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

type expectation =
  | Exact of Type.t
  | Base
  | Stream
  | Prod
  | Fun
  | Sub of expectation

type infer_kind =
  | Eq of Scoped_tree.T.eq
  | Pat of Scoped_tree.T.pat

type typing_error =
  | Type_clash of { expected : expectation; actual : Type.t; loc : Loc.loc; }
  | Cannot_infer of { kind : infer_kind; loc : Loc.loc; }
  | Cannot_coerce of { ty : Type.t; coe : Coercion.t; loc : Loc.loc; }
  | Ill_typed_pat of { pat : Scoped_tree.T.pat; expected : Type.t; }
  | Not_a_subtype of { ty1 : Type.t; ty2 : Type.t;
                       clash_ty1 : Type.t; clash_ty2 : Type.t;
                       loc : Loc.loc; }

exception Typing_error of typing_error

val print_typing_error : typing_error Warp.Print.printer

val pass : (Scoped_tree.T.file -> Typed_tree.T.file) Pass.t

val serialize : (Typed_tree.T.file -> Typed_tree.T.file) Pass.t
