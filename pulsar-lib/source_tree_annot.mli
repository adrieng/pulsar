(* This file is part of Pulsar, a temporal functional language.
 * Copyright (C) 2017,2018 Adrien Guatto
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
