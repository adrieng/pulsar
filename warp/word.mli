type elem = int

type t

val empty : t

val singleton : elem -> t

val concat : t list -> t

val (^^) : t -> t -> t

val power : t -> int -> t

val of_list : int list -> t

val split_at : int -> t -> t * t

val drop : int -> t -> t

val rotate : t -> t

val rev : t -> t

val at : t -> int -> elem

val length : t -> int

val weight : t -> elem

val is_empty : t -> bool

val has_null_weight : t -> bool

val all_equal : elem -> t -> bool

include Utils.PrintableOrderedType with type t := t

val equal : t -> t -> bool
