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

module Diagnostic =
struct
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

  let print_kind fmt k =
    match k with
    | Error ->
       Format.fprintf fmt "error"
    | Warning ->
       Format.fprintf fmt "warning"
    | Info ->
       Format.fprintf fmt "info"

  let print fmt { loc; pass; kind; body; } =
    Format.fprintf fmt "@[<2>%a%s[%a] %s:@ %a@]@\n"
      Loc.print loc
      (if loc <> Loc.nowhere then " " else "")
      print_kind kind
      pass
      Warp.Print.pp_thunk body

  let make ~loc ~kind ~body =
    {
      loc = loc;
      pass = !Prop.pass;
      kind = kind;
      body = body;
    }

  let rcb = ref (fun _ -> ())

  let on_diagnostic cb =
    rcb := cb

  exception Error of t

  let error ?(loc = Loc.nowhere) ~body () =
    let diag = make ~loc ~kind:Error ~body in
    !rcb diag;
    raise (Error diag)

  let warning ?(loc = Loc.nowhere) ~body () =
    let diag = make ~loc ~kind:Warning ~body in
    !rcb diag

  let info ?(loc = Loc.nowhere) ~body () =
    let diag = make ~loc ~kind:Info ~body in
    !rcb diag
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
    let output_file =
      if !Options.serialize_dir = ""
      then output_file
      else Filename.(!Options.serialize_dir ^ dir_sep ^ basename output_file)
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

  type 'a result =
    | Correct of 'a
    | Error of Diagnostic.t

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
      Correct y
    with Diagnostic.Error err ->
      Error err

  let run p x =
    let rec loop : type a b. (a -> b) t -> a -> b result =
      fun p x ->
      match p with
      | Atomic at ->
         run_atomic at x
      | Seq (p1, p2) ->
         begin match loop p1 x with
         | Error diag ->
            Error diag
         | Correct y ->
            loop p2 y
         end
    in
    loop p x

  let rec names : type a. a t -> string list =
    fun p ->
    match p with
    | Atomic { name; _ } ->
       [name]
    | Seq (p1, p2) ->
       names p1 @ names p2

  let command_line p =
    [
      "-debug-pass",
      Arg.Symbol (names p, Options.set_debug),
      " display debugging information for pass";

      "-serialize",
      Arg.Symbol (names p, Options.set_serialize),
      " serialize the output of pass to file.pass.ext";

      "-serialize-dir",
      Arg.String (fun s -> Options.serialize_dir := s),
      " serialization directory";

      "-stop-before",
      Arg.Symbol (names p, Options.set_stop_before),
      " stop before running pass";

      "-stop-after",
      Arg.Symbol (names p, Options.set_stop_after),
      " stop after running pass";
    ]
end
