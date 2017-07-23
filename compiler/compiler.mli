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

module Message :
sig
  type kind =
    | Error
    | Warning
    | Info

  type t =
    {
      m_loc : Loc.loc;
      m_kind : kind;
      m_text : string;
    }

  val make : ?loc:Loc.loc -> kind:kind -> text:string -> t

  val error : ?loc:Loc.loc -> text:string -> t

  val warning : ?loc:Loc.loc -> text:string -> t

  val info : ?loc:Loc.loc -> text:string -> t
end

module Prop :
sig
  type _ t =
    | File : string t
    | Pass : string t

  val get : 'a t -> 'a

  val set_file : string -> unit
end

module Pass :
sig
  type 'a t

  val atomic :
    ?pp_in : 'a Warp.Print.printer ->
    ?pp_out : 'b Warp.Print.printer ->
    name:string ->
    ('a -> 'b) ->
    ('a -> 'b) t

  val ( >>> ) : ('a -> 'b) t -> ('b -> 'c) t -> ('a -> 'c) t

  val run : ('a -> 'b) t -> 'a -> 'b

  val command_line : 'a t -> (string * Arg.spec * string) list
end
