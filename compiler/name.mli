(** Module name. *)
type modname = string

(** Top-level declaration name. *)
type shortname = string

(** Fully qualified name. *)
type t = private { modname : modname; name : shortname; }

include Warp.Utils.PrintableOrderedType with type t := t

val make : modname:modname -> name:shortname -> t
