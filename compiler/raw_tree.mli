(** Raw AST with string as identifiers *)
module T : Source_tree.Tree with type Id.t = string
                             and type PatAnnot.t = unit
                             and type ExpAnnot.t = unit
                             and type EquAnnot.t = unit
                             and type PhrAnnot.t = unit

(** Helper functions for easy AST construction *)

val make_app : T.exp -> T.exp -> T.exp
