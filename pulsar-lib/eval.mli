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

open Typed_tree.T

(** {2 Values and Environments} *)

type value =
  | Vnil
  | Vconst of Const.const
  | Vscons of value * value
  | Vpair of value * value
  | Vclo of pat * exp * env
  | Vwarp of Warp.Formal.t * value
  | Vthunk of exp * env

and env =
  value Ident.Env.t

val print_value : value Warp.Print.printer

val print_env : env Warp.Print.printer

(** {2 Errors} *)

type tag_error =
  | TE_coercion of coe * value
  | TE_pattern of pat * value
  | TE_expression of exp * string

exception Tag_error of tag_error

val print_tag_error : tag_error Warp.Print.printer

(** {2 Evaluation} *)

val restrict : Warp.Enat.t -> value -> value

val eval_coe : Warp.Enat.t -> coe -> value -> value

val eval_exp : Warp.Enat.t -> exp -> env -> value

val eval_eq : Warp.Enat.t -> i_env:env -> e_env:env -> eq -> env
