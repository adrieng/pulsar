type invertible =
  | Wrap
  | Unwrap
  | Concat of Warp_type.t * Warp_type.t
  | Decat of Warp_type.t * Warp_type.t
  | Dist
  | Fact
  | Infl
  | Defl

type t =
  | Id
  | Seq of t * t
  | Arr of t * t
  | Prod of t * t
  | Warped of Warp_type.t * t
  | Invertible of invertible
  | Delay of Warp_type.t * Warp_type.t

val compare_invertible : invertible -> invertible -> int

val equal_invertible : invertible -> invertible -> bool

val print_invertible : Format.formatter -> invertible -> unit

val compare : t -> t -> int

val equal : t -> t -> bool

val print : Format.formatter -> t -> unit
