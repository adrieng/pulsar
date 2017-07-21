type t

val print : Format.formatter -> t -> unit

val compare : t -> t -> int

val equal : t -> t -> bool

val is_unit : t -> bool

val unit : t

val on : t -> t -> t

val make : Periodic.t -> t

val normalize : t -> Periodic.t

(** The warp (1). *)
val one : t

(** The warp (omega). *)
val omega : t

(** The warp 0(1). *)
val zero_one : t

val ( <= ) : t -> t -> bool

val div : t -> t -> t
