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

module Prop =
struct
  type _ t =
    | File : string t
    | Pass : string t

  let file = ref ""

  let pass = ref ""

  let get : type a. a t -> a =
    fun p ->
    match p with
    | File -> !file
    | Pass -> !pass

  let set_file fn =
    file := fn
end

module Message =
struct
  type kind =
    | Error
    | Warning
    | Info

  type t =
    {
      m_loc : Loc.loc;
      m_pass : string;
      m_kind : kind;
      m_text : string;
    }

  let print_kind fmt k =
    match k with
    | Error ->
       Format.fprintf fmt "error"
    | Warning ->
       Format.fprintf fmt "warning"
    | Info ->
       Format.fprintf fmt "info"

  let print fmt { m_loc; m_pass; m_kind; m_text; } =
    Format.fprintf fmt "@[<h>%a%s[%a] %s:@ %s@]"
      Loc.print_loc m_loc
      (if m_loc <> Loc.nowhere then " " else "")
      print_kind m_kind
      m_pass
      m_text

  let make ~loc ~kind ~text =
    {
      m_loc = loc;
      m_pass = !Prop.pass;
      m_kind = kind;
      m_text = text;
    }

  exception Error of t

  let error ?(loc = Loc.nowhere) ~text () =
    raise (Error (make ~loc ~kind:Error ~text))

  let warning ?(loc = Loc.nowhere) ~text () =
    Format.eprintf "%a@." print (make ~loc ~kind:Warning ~text)

  let info ?(loc = Loc.nowhere) ~text () =
    Format.printf "%a@." print (make ~loc ~kind:Info ~text)
end

module Pass =
struct
  type ('a, 'b) atomic_pass =
    {
      name : string;
      pp_in : 'a Warp.Print.printer;
      pp_out : 'b Warp.Print.printer;
      body : 'a -> 'b;
    }

  type _ t =
    | Atomic : ('a, 'b) atomic_pass -> ('a -> 'b) t
    | Seq : ('a -> 'b) t * ('b -> 'c) t -> ('a -> 'c) t

  let atomic
        ?(pp_in = Warp.Print.pp_nothing)
        ?(pp_out = Warp.Print.pp_nothing)
        ~name
        body =
    Atomic
      {
        name;
        pp_in;
        pp_out;
        body;
      }

  let ( >>> ) p1 p2 =
    Seq (p1, p2)

  let run_atomic at x =
    if !Options.debug || Options.pass_debug at.name
    then Format.eprintf "(* Running pass %s *)@." at.name;
    Prop.pass := at.name;
    try
      let y = at.body x in
      if !Options.debug || Options.pass_debug at.name
      then
        begin
          Format.eprintf "(* Finished running %s *)@." at.name;
          Format.eprintf "%a@." at.pp_out y;
        end;
      y
    with Message.Error err ->
         Format.eprintf "%a@."Message.print err;
         exit 1

  let run p x =
    let rec loop : type a b. (a -> b) t -> a -> b =
      fun p x ->
      match p with
      | Atomic at ->
         run_atomic at x
      | Seq (p1, p2) ->
         let y = loop p1 x in
         loop p2 y
    in
    loop p x

  let command_line p =
    let rec pass_names : type a. a t -> string list =
      fun p ->
      match p with
      | Atomic { name; _ } ->
         [name]
      | Seq (p1, p2) ->
         pass_names p1 @ pass_names p2
    in
    [
      "-debug-pass",
      Arg.Symbol (pass_names p, Options.set_debug),
      " display debugging information for pass";
    ]
end
