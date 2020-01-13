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

(** Identifiers with unique identity *)
type t

(** Every ident is associated with exactly one context *)
type ctx

(** Create a new context with no associated ident *)
val make_ctx : unit -> ctx

(** Get the current ambient context *)
val get_current_ctx : unit -> ctx

(** Set the current ambient context *)
val set_current_ctx : ctx -> unit

(** Create a new context and set it as the current ambient context *)
val reset_ctx : unit -> unit

(** Create a fresh identifier coming from a source file with the given name in
    the ambient context *)
val make_source : string -> t

(** Create a fresh identifier for internal compiler use with the given name in
    the ambient context *)
val make_internal : string -> t

(** Create a fresh identifier by appending the given prefix to the name of an
    already existing one *)
val make_suffix : t -> string -> t

(** Create a fresh identifier by appending the given prefix to the name of an
    already existing one *)
val make_prefix : string -> t -> t

(** Create a fresh identifier with the same name and source/internal status as
    an existing one *)
val refresh : t -> t

(** Comparison function for idents a la [Pervasives.compare] *)
val compare : t -> t -> int

(** Hashing function for identifieirs a la [Pervasives.hash] *)
val hash : t -> int

(** Equality testing for idents; fast *)
val equal : t -> t -> bool

(** Sets of idents with printing *)
module Set :
  sig
    include Set.S with type elt = t
    val unions : t list -> t
    val print : Format.formatter -> t -> unit
  end

(** Extended ident-indexed maps *)
module Env :
  sig
    include Warp.Utils.ExtMap with type key = t
    val trim : 'a t -> Set.t -> 'a t
    val mapfold :
      (key * 'a -> 'b -> (key * 'a) * 'b) ->
      'a t ->
      'b ->
      'a t * 'b
    val mapfold_elems :
      ('a -> 'b -> 'a * 'b) ->
      'a t ->
      'b ->
      'a t * 'b
    val print : ?sep:string -> 'a Warp.Print.printer -> 'a t Warp.Print.printer
  end

(** Pretty-print an identifier. Guaranteed to be injective for all the
    idents created in the same context. *)
val print : Format.formatter -> t -> unit

(** Pretty-print an identifier as it appears in source code. *)
val print_source : Format.formatter -> t -> unit

(** Turn an identifier into a concrete string. Guaranteed to be injective for
    all the idents created in the same context. *)
val to_string : t -> string
