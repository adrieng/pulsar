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
