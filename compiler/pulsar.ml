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

open Parse
open Parser
open Compiler

let handle_internal_errors f x =
  try f x
  with
  | Lexer.Lexing_error err ->
    if !Options.debug_lexing then Format.eprintf "@\n";
    Format.eprintf "%a@." Lexer.print_lexing_error err
  | Parser_error.Parsing_error err ->
    if !Options.debug_lexing then Format.eprintf "@\n";
    Format.eprintf "%a@." Parser_error.print_parsing_error err
  | Scoping.Scoping_error err ->
    Format.eprintf "%a@." Scoping.print_scoping_error err
  | Typing.Typing_error err ->
    Format.eprintf "%a@." Typing.print_typing_error err

let frontend =
  let open Pass in
  Parse.pass
  >>> Scoping.pass
  >>> Typing.pass
  >>> Typing.serialize

let compiler =
  frontend

let process_pulsar_file filename =
  Prop.set_file filename;
  let ic = open_in filename in
  ignore @@ Pass.run compiler ic

let args =
  Options.global_command_line_arguments
  @ Pass.command_line compiler

let usage = "pulsar <options> <files>"

let files = ref []

let process filename =
  match Warp.Utils.file_extension filename with
  | ".pul" ->
    handle_internal_errors process_pulsar_file filename
  | _ ->
    Printf.eprintf "%s: don't know what to do with %s\n"
      Sys.argv.(0)
      filename;
    Arg.usage args usage;
    exit 1

let _ =
  Arg.parse args (fun s -> files := s :: !files) usage;
  List.iter process (List.rev !files)
