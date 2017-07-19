(** Operators *)
type op =
  | Plus
  | Minus
  | Times
  | Div

(** Pretty-print an operator *)
val print_op : Format.formatter -> op -> unit

(** Comparison function for operator a la [Pervasives.compare] *)
val compare_op : op -> op -> int

(** Priority of the operation *)
val priority : op -> int

(** Constants *)
type const =
  | Lit of Scal.t
  | Op of op
  | When of Warp.Periodic.t
  | Merge of Warp.Periodic.t

(** Pretty-print a constant *)
val print_const : Format.formatter -> const -> unit

(** Comparison function for constants a la [Pervasives.compare] *)
val compare_const : const -> const -> int

(** Compute the type of a constant. *)
val type_of : const -> Type.t
