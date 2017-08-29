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

type t =
  | Seq of t * t
  | Arr of t * t
  | Prod of t * t
  | Warped of Warp.Formal.t * t
  | Invertible of Invertible.t
  | Delay of Warp.Formal.t * Warp.Formal.t

include Warp.Utils.PrintableOrderedType with type t := t

val id : t

(** [try_invert coe] tries to invert the coercion coe. It raises
    invalid_argument if this is not possible. *)
val try_invert : t -> t

(** {2 Typing} *)

exception Ill_typed

val output_type : t -> Type.t -> Type.t

val input_type : t -> Type.t -> Type.t

(** {2 Equational theory} *)

(** {3 Smart constructors} *)

val seq : t * t -> t

val seqs : t list -> t

val arr : t * t -> t

val prod : t * t -> t

val warped : Warp.Formal.t * t -> t

val invertible : Invertible.t -> t

val delay : Warp.Formal.t * Warp.Formal.t -> t

(** {3 Coercion reduction} *)

val reduce : t -> t
