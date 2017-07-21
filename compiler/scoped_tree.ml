module T = Source_tree.Make(
  struct
    module Id =
      struct
        type t = Ident.t
        let print = Ident.print_source
        let compare = Ident.compare
      end

    module PatAnnot = Warp.Utils.PrintableOrderedUnit
    module ExpAnnot = Warp.Utils.PrintableOrderedUnit
    module EquAnnot = Warp.Utils.PrintableOrderedUnit
    module PhrAnnot = Warp.Utils.PrintableOrderedUnit
    module FileAnnot = Warp.Utils.PrintableOrderedUnit
  end
)

module V = Source_tree_utils.Vars(T)
