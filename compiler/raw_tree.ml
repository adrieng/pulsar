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

module T = Source_tree.Make(
  struct
    module Id = Warp.Utils.PrintableOrderedString

    module PatAnnot = Warp.Utils.PrintableOrderedUnit
    module ExpAnnot = Warp.Utils.PrintableOrderedUnit
    module EquAnnot = Warp.Utils.PrintableOrderedUnit
    module PhrAnnot = Warp.Utils.PrintableOrderedUnit
    module FileAnnot = Warp.Utils.PrintableOrderedUnit
  end
)

open T

let join_loc e1 e2 =
  Loc.join e1.e_loc e2.e_loc

let make_app e1 e2 =
  {
    e_desc = EApp (e1, e2);
    e_loc = join_loc e1 e2;
    e_ann = ();
  }
