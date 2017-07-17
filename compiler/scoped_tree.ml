module T = Source_tree.Make(
  struct
    module Id = Ident

    module PatAnnot = Warp.Utils.PrintableOrderedUnit
    module ExpAnnot = Warp.Utils.PrintableOrderedUnit
    module EquAnnot = Warp.Utils.PrintableOrderedUnit
    module PhrAnnot = Warp.Utils.PrintableOrderedUnit
  end
)
