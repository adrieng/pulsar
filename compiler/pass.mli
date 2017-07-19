(** {2 Compiler configuration} *)

type ctx

val make_default : filename:string -> ctx

val current_file : ctx -> string

(** {2 Passes} *)

type 'a t

val atomic :
  ?pp_in : 'a Warp.Print.printer ->
  ?pp_out : 'b Warp.Print.printer ->
  name:string ->
  (ctx -> 'a -> 'b) ->
  ('a -> 'b) t

val ( >>> ) : ('a -> 'b) t -> ('b -> 'c) t -> ('a -> 'c) t

val run : ctx:ctx -> ('a -> 'b) t -> 'a -> 'b

val command_line_arguments : 'a t -> (string * Arg.spec * string) list
