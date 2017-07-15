(** Types of stream elements *)
type base =
  | Unit
  | Bool
  | Char
  | Int
  | Float

(** Pretty-print a base type *)
val print_base : Format.formatter -> base -> unit

(** Comparison function for base types a la [Pervasives.compare]. *)
val compare_base : base -> base -> int

(** Equality testing for base types *)
val equal_base : base -> base -> bool

(** General types *)
type t =
  | Base of base
  | Stream of base
  | Prod of t * t
  | Fun of t * t
  | Warped of Warp_type.t * t

(** Prett-print a tpe *)
val print : Format.formatter -> t -> unit

(** Compute the normal form of a tpe w.r.t. reversible tpe laws *)
val normalize : t -> t

(** Comparison function for tpes a la [Pervasives.compare] *)
val compare : t -> t -> int

(** Equalit testing for tpes *)
val equal : t -> t -> bool

(** Equivalence testing for tpes w.r.t. reversible tpe laws *)
val equiv : t -> t -> bool
