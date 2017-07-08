(* Copyright (C) Adrien Guatto <adrien.guatto@laposte.net> 2013-2014
 *
 * This file is part of Acid Synchrone.
 *
 * nsched is free software: you can redistribute it and/or modify it under the
 * terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any later
 * version.
 *
 * nsched is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
 * A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * nsched. If not, see <http://www.gnu.org/licenses/>.
 *)

type t =
  private
    {
      num : Int.t;
      den : Int.t;
    }

val print : Format.formatter -> t -> unit

val make : Int.t -> Int.t -> t

val int : Int.t -> t

val to_float : t -> float

val ( = ) : t -> t -> bool

val ( <= ) : t -> t -> bool

val ( < ) : t -> t -> bool

val ( >= ) : t -> t -> bool

val ( > ) : t -> t -> bool

val neg : t -> t

val inv : t -> t

val ( + ) : t -> t -> t

val ( - ) : t -> t -> t

val ( * ) : t -> t -> t

val ( / ) : t -> t -> t
