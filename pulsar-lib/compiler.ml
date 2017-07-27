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
      m_body : unit Warp.Print.printer;
    }

  let print_kind fmt k =
    match k with
    | Error ->
       Format.fprintf fmt "error"
    | Warning ->
       Format.fprintf fmt "warning"
    | Info ->
       Format.fprintf fmt "info"

  let print fmt { m_loc; m_pass; m_kind; m_body; } =
    Format.fprintf fmt "@[%a%s[%a] %s:@ %a@]"
      Loc.print_loc m_loc
      (if m_loc <> Loc.nowhere then " " else "")
      print_kind m_kind
      m_pass
      Warp.Print.pp_thunk m_body

  let make ~loc ~kind ~body =
    {
      m_loc = loc;
      m_pass = !Prop.pass;
      m_kind = kind;
      m_body = body;
    }

  exception Error of t

  let error ?(loc = Loc.nowhere) ~body () =
    raise (Error (make ~loc ~kind:Error ~body))

  let warning ?(loc = Loc.nowhere) ~body () =
    Format.eprintf "%a@." print (make ~loc ~kind:Warning ~body)

  let info ?(loc = Loc.nowhere) ~body () =
    Format.printf "%a@." print (make ~loc ~kind:Info ~body)
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

  let serialize at y =
    let input_file = Prop.(get File) in
    let output_file =
      Printf.sprintf "%s.%s.pul"
        (Filename.chop_extension input_file)
        at.name
    in
    let oc = open_out output_file in
    let fmt = Format.formatter_of_out_channel oc in
    let tm = Unix.(localtime @@ time ()) in
    Format.fprintf fmt "(* %s generated %.2d:%.2d:%.2d %.2d/%.2d/%d *)@\n"
      output_file
      tm.tm_hour tm.tm_min tm.tm_sec
      tm.tm_mon tm.tm_mday (1900 + tm.tm_year);
    Format.fprintf fmt "(* command-line: %a *)@\n"
      Warp.Print.(pp_array ~pp_sep:pp_space pp_string) Sys.argv;
    Format.fprintf fmt "%a@?" at.pp_out y;
    close_out oc
  ;;

  let run_atomic at x =
    Prop.pass := at.name;
    if Options.pass_stop_before at.name then exit 0;
    if !Options.debug || Options.pass_debug at.name
    then Format.eprintf "(* Running pass %s *)@." at.name;
    try
      let time, y = Warp.Utils.time_call at.body x in
      if !Options.debug || Options.pass_debug at.name
      then
        begin
          Format.eprintf "(* Finished running %s in %f seconds *)@."
            at.name
            time;
          Format.eprintf "%a@?" at.pp_out y;
        end;
      if Options.pass_serialize at.name then serialize at y;
      if Options.pass_stop_after at.name then exit 0;
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
      "-serialize",
      Arg.Symbol (pass_names p, Options.set_serialize),
      " serialize the output of pass to file.pass.ext";
      "-stop-before",
      Arg.Symbol (pass_names p, Options.set_stop_before),
      " stop before running pass";
      "-stop-after",
      Arg.Symbol (pass_names p, Options.set_stop_after),
      " stop after running pass";
    ]
end
