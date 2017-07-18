type scoping_error =
  | Unbound_identifier of string * Loc.loc

exception Scoping_error of scoping_error

val print_scoping_error : scoping_error Warp.Print.printer

val pass : (Raw_tree.T.file -> Scoped_tree.T.file) Pass.t
