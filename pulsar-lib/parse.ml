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

let parse_pulsar_file filename =
  Compiler.Prop.set_file filename;
  let ic =
    try open_in filename
    with Sys_error msg ->
      let body fmt () =
        Format.fprintf fmt "Could not open file %s\n%s" filename msg
      in
      Compiler.Diagnostic.error ~body ()
  in
  let lex = Lexer.ctx_from_utf8_channel ~filename ic in
  let supplier () =
    let tok, start, stop = Lexer.next_token_pos lex in
    if !Options.debug
    then Format.eprintf "%a @?" Lexer.print_token tok;
    tok, start, stop
  in
  let chk =
    let initial_pos =
      Lexing.{ pos_fname = filename;
               pos_lnum = 0;
               pos_bol = 0;
               pos_cnum = 0; }
    in
    Parser.Incremental.file initial_pos
  in
  let file = Parser.MenhirInterpreter.loop supplier chk in
  close_in ic;
  file

let pass =
  Compiler.Pass.atomic
    ~pp_out:Raw_tree.T.print_file
    ~name:"parsing"
    parse_pulsar_file
