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
    module Id =
      struct
        type t = Ident.t
        let print = Ident.print_source
        let compare = Ident.compare
      end

    module PatAnnot = Warp.Utils.PrintableOrderedUnit
    module CoeAnnot = Warp.Utils.PrintableOrderedUnit
    module ExpAnnot = Warp.Utils.PrintableOrderedUnit
    module EquAnnot = Warp.Utils.PrintableOrderedUnit
    module PhrAnnot = Warp.Utils.PrintableOrderedUnit
    module FileAnnot = Warp.Utils.PrintableOrderedUnit
  end
)

module V = Source_tree_utils.Vars(T)
