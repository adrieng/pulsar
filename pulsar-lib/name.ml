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

(** Module name. *)
type modname = string

(** Top-level declaration name. *)
type shortname = string

(** Fully qualified name. *)
type t = { modname : modname; name : shortname; }

let print fmt { modname; name; } =
  Format.fprintf fmt "%s.%s" modname name

let compare q1 q2 =
  Warp.Utils.compare_both
    (Warp.Utils.compare_string q1.modname q2.modname)
    (fun () -> Warp.Utils.compare_string q1.name q2.name)

(* TODO check that modname conformance. *)
let make ~modname ~name =
  {
    modname;
    name;
  }
