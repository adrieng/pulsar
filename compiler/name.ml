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

let print fmt { qual; name; } =
  match qual with
  | Current ->
     Format.fprintf fmt "%s" name
  | Module modn ->
     Format.fprintf fmt "%s.%s" modn name

let compare_modqual q1 q2 =
  let tag_to_int q =
    match q with
    | Current -> 0
    | Module _ -> 1
  in
  match q1, q2 with
  | Current, Current ->
     0
  | Module n1, Module n2 ->
     Warp.Utils.compare_string n1 n2
  | (Current | Module _), _ ->
     Warp.Utils.compare_int (tag_to_int q1) (tag_to_int q2)

let compare { qual = q1; name = n1; } { qual = q2; name = n2; } =
  Warp.Utils.compare_both
    (compare_modqual q1 q2)
    (fun () -> Warp.Utils.compare_string n1 n2)
