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

module type S =
  sig
    (** Warps *)
    type t

    include Utils.PrintableOrderedType with type t := t

    val equal : t -> t -> bool

    (** Warp composition *)
    val on : t -> t -> t

    (** Warp division *)
    val div : t -> t -> t

    (** Precedence test *)
    val ( <= ) : t -> t -> bool

    (** The warp represented by (0) *)
    val zero : t

    (** The warp represented by (1) *)
    val one : t

    (** The warp represented by (\omega) *)
    val omega : t

    (** The warp represented by 0(1) *)
    val zero_one : t
  end
