(** {1 Simple sequence library} *)

(** This is strongly inspired from https://github.com/c-cube/sequence/ *)

type 'a t = ('a -> unit) -> unit
