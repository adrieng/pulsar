type t =
  | Inf
  | Fin of int

include Utils.PrintableOrderedType with type t := t

val equal : t -> t -> bool

val zero : t

val one : t

val succ : t -> t

val ( <= ) : t -> t -> bool

val ( + ) : t -> t -> t

val ( - ) : t -> t -> t

val ( * ) : t -> t -> t

val ( / ) : t -> t -> t

val min : t -> t -> t

val max : t -> t -> t

val of_int : int -> t

exception Too_big

val to_int : t -> int

val print_utf8 : bool ref
