(** Scalar values *)
type t =
  | Bool of bool
  | Char of char
  | Int of int
  | Float of float

(** Pretty-print a scalar value *)
val print : Format.formatter -> t -> unit

(** Comparison function for scalars a la [Pervasive.compare] *)
val compare : t -> t -> int

(** Compute the type of a scalar value, which is always a base type *)
val type_of : t -> Types.bty
