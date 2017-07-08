type t

val print : Format.formatter -> t -> unit

val compare : t -> t -> int

val is_unit : t -> bool

val unit : t

val on : t -> t -> t

val make : Clock.Periodic.t -> t
