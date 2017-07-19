type t

val print : Format.formatter -> t -> unit

val compare : t -> t -> int

val equal : t -> t -> bool

val is_unit : t -> bool

val unit : t

val on : t -> t -> t

val make : Warp.Periodic.t -> t

val normalize : t -> Warp.Periodic.t

val one : t

val omega : t

val ( <= ) : t -> t -> bool

val div : t -> t -> t
