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

open Flow
open Message

module U = Warp.Utils
module J = Yojson.Basic

let decode line =
  try U.Left (Request.of_json @@ J.from_string line)
  with Yojson.Json_error reason ->
       U.Right (Response.Decoding { reason; })
     | Message.Could_not_decode { reason; json; } ->
       let reason = reason ^ " at " ^ J.to_string json in
       U.Right (Response.Decoding { reason; })

let encode resp =
  J.to_string @@ Response.to_json resp

let diag_callback diag =
  Process.diagnoses := diag :: !Process.diagnoses;
  ()

let rec do_req req_str =
  let resp =
    match decode req_str with
    | U.Left req ->
       Process.process req
    | U.Right ko ->
       Ko ko
  in
  print_string @@ encode resp;
  ()

let _ =
  Compiler.Diagnostic.on_diagnostic diag_callback;
  for i = 1 to Array.length Sys.argv - 1 do
    do_req Sys.argv.(i)
  done
