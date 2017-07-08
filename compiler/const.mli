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

(** Constants *)
type const =
  | Lit of Scal.t
  | Op of op
  | When of Clock.Periodic.t
  | Merge of Clock.Periodic.t

(** Pretty-print a constant *)
val print_const : Format.formatter -> const -> unit

(** Comparison function for constants a la [Pervasives.compare] *)
val compare_const : const -> const -> int
