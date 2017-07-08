(** Raw AST with string as identifiers *)
module T : Source_tree.Tree with type id := string and type ann := unit

(** Helper functions for easy AST construction *)

val make_app : T.exp -> T.exp -> T.exp
