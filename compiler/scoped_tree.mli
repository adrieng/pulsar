(** Scoped AST with idents as identifiers *)
module T : Source_tree.Tree with type Id.t = Ident.t
                             and type PatAnnot.t = unit
                             and type ExpAnnot.t = unit
                             and type EquAnnot.t = unit
                             and type PhrAnnot.t = unit
