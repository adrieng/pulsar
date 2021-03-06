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

module Prop :
sig
  type _ t =
    | File : string t
    | Pass : string t

  val get : 'a t -> 'a

  val set_file : string -> unit
end

module Diagnostic :
sig
  type kind =
    | Error
    | Warning
    | Info

  type t =
    {
      loc : Loc.t;
      pass : string;
      kind : kind;
      body : unit Warp.Print.printer;
    }

  val print : Format.formatter -> t -> unit

  val on_diagnostic : (t -> unit) -> unit

  (** The following functions can be called by passes to communicate messages to
      the user. The [error] function never returns. *)

  val error : ?loc:Loc.t -> body:unit Warp.Print.printer -> unit -> 'a

  val warning : ?loc:Loc.t -> body:unit Warp.Print.printer -> unit -> unit

  val info : ?loc:Loc.t -> body:unit Warp.Print.printer -> unit -> unit
end

module Pass :
sig
  (** The type of passes. *)
  type 'a t

  (** {3 Pass combinators} *)

  val atomic :
    ?pp_in : 'a Warp.Print.printer ->
    ?pp_out : 'b Warp.Print.printer ->
    name:string ->
    ('a -> 'b) ->
    ('a -> 'b) t

  val ( >>> ) : ('a -> 'b) t -> ('b -> 'c) t -> ('a -> 'c) t

  (** {3 Executing passes} *)

  type 'a result =
    | Correct of 'a
    | Error of Diagnostic.t

  val run : ('a -> 'b) t -> 'a -> 'b result

  (** {3 Misc} *)

  (** [names p] returns the name of the atomic passes included in the pass
  [p]. *)
  val names : 'a t -> string list

  (** [command_line p] computes the command-line options for the pass [p] in the
      format understood by the Arg module. *)
  val command_line : 'a t -> (string * Arg.spec * string) list
end
