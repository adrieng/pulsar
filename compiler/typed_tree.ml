module T = Source_tree.Make(
  struct
    module Id =
      struct
        type t = Ident.t
        let print = Ident.print_source
        let compare = Ident.compare
      end

    module PatAnnot = Type
    module ExpAnnot = Type
    module EquAnnot = Type
    module PhrAnnot = Type
  end
)
