(* This file is part of Pulsar, a temporal functional language.
 * Copyright (C) 2017-2020 Adrien Guatto
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

(** This functor performs a simple form of hash consing on a type of names,
    resulting in a new type where equality can be tested in constant time. A
    typical use case is to turn strings into efficient identifiers. *)
module Make(Name : Wdr.ExtSigs.OrderedPrintableHashed0) : sig
  (** The type of {b identifiers}, which can be compared efficiently. *)
  type t

  include Hashtbl.HashedType with type t := t

  (** [fresh name] returns a fresh identifier with name [name]. *)
  val fresh : Name.t -> t

  (** [name x] returns the name of identifier [x]. *)
  val name : t -> Name.t

  (** [print x] print a non-ambiguous representation of [x]. *)
  val print : t -> PPrint.document
end
