(* This file is part of Pulsar, a temporal functional language.
 * Copyright (C) 2017 Adrien Guatto
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the LICENSE file in the top-level directory.
 *)

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
