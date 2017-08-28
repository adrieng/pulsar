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

let usage = "pulsar <options> <files>"

let files = ref []

let diagnostic_callback diag =
  Format.eprintf "%a@?" Compiler.Diagnostic.print diag

let args = Flow.common_args

let process filename =
  match Filename.extension filename with
  | ".pul" ->
     let open Compiler.Pass in
     let res = run Flow.compiler filename in
     begin match res with
     | Correct _ ->
        ()
     | Error _ ->
        exit 1
     end
  | _ ->
    Printf.eprintf "%s: don't know what to do with %s\n"
      Sys.argv.(0)
      filename;
    Arg.usage args usage;
    exit 1

let _ =
  Compiler.Diagnostic.on_diagnostic diagnostic_callback;
  Arg.parse args (fun s -> files := s :: !files) usage;
  List.iter process (List.rev !files)
