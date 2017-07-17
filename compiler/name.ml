(** Module name. *)
type modname = string

(** Top-level declaration name. *)
type shortname = string

(** Fully qualified name. *)
type t = { modname : modname; name : shortname; }

let print fmt { modname; name; } =
  Format.fprintf fmt "%s.%s" modname name

let compare q1 q2 =
  Warp.Utils.compare_both
    (Warp.Utils.compare_string q1.modname q2.modname)
    (fun () -> Warp.Utils.compare_string q1.name q2.name)

(* TODO check that modname conformance. *)
let make ~modname ~name =
  {
    modname;
    name;
  }
