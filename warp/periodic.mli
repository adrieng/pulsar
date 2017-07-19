type extremal =
  | Zero
  | Omega

type t

(** Construction and destruction *)

val make_extremal : ?prefix:Word.t -> extremal -> t

val make_pattern : ?prefix:Word.t -> ppattern:Word.t -> t

(** Indexed access and represented clock *)

val at : t -> int -> Enat.t

val ones : t -> Enat.t -> Enat.t

(** Special clocks *)

val one : t

val zero : t

val omega : t

val ladj : t -> t

val radj : t -> t

val div : t -> t -> t

(** Algebra *)

val weight : t -> Enat.t

val is_finitary : t -> bool

val is_stuck : t -> bool

val on : t -> t -> t

(** Equality, comparison, printing *)

val is_one : t -> bool

val equal : t -> t -> bool

val print_utf8 : bool ref

include Utils.PrintableOrderedType with type t := t

val quick_normalize : t -> t

(** Extensional order *)

val ( <= ) : t -> t -> bool
