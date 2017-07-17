(** Module name. *)
type modname = string

(** Module qualifier, can be either the current module or some named module. *)
type modqual =
  | Current
  | Module of string

(** Top-level declaration name. *)
type shortname = string

(** Fully qualified name. *)
type t = { qual : modqual; name : shortname; }

val print : t Warp.Print.printer

val compare : t -> t -> int
