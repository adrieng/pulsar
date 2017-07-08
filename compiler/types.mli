(** Types of stream elements *)
type bty =
  | Bool
  | Char
  | Int
  | Float

(** Pretty-print a base type *)
val print_base_ty : Format.formatter -> bty -> unit

(** Comparison function for base types a la [Pervasives.compare]. *)
val compare_bty : bty -> bty -> int

(** Equality testing for base types *)
val equal_bty : bty -> bty -> bool

(** General types *)
type ty =
  | Unit
  | Stream of bty
  | Prod of ty * ty
  | Fun of ty * ty
  | Box of Clock_type.t * ty

(** Pretty-print a type *)
val print_ty : Format.formatter -> ty -> unit

(** Compute the normal form of a type w.r.t. reversible type laws *)
val normalize : ty -> ty

(** Comparison function for types a la [Pervasives.compare] *)
val compare_ty : ty -> ty -> int

(** Equality testing for types *)
val equal_ty : ty -> ty -> bool

(** Equivalence testing for types w.r.t. reversible type laws *)
val equiv_ty : ty -> ty -> bool
