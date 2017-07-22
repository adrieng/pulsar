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

(* Utility functions *)

let yes_no ~opt ~msg r =
  let activate s =
    match s with
    | "yes" ->
       r := true
    | "no" ->
       r := false
    | _ ->
       assert false
  in
  "-" ^ opt,
  Arg.Symbol (["yes"; "no"], activate),
  " " ^ msg ^ " (default: " ^ (if !r then "yes" else "no") ^ ")"

(* Debugging-related flags *)

let debug = ref false

let debug_lexing = ref false

let debug_parsing = ref false

let debug_scoping = ref false

let debug_typing = ref false

let debug_table =
  [
    "lexing", debug_lexing;
    "parsing", debug_parsing;
    "scoping", debug_scoping;
    "typing", debug_typing;
  ]

let set_debug s =
  try
    let r = List.assoc s debug_table in
    r := true
  with Not_found ->
    invalid_arg ("set_debug: " ^ s)

let pass_debug s =
  try
    let r = List.assoc s debug_table in
    !r
  with Not_found ->
    false

(* Misc global options *)

let display_types = ref false

let auto_const = ref true

(* Command-line arguments related to global options *)

let global_command_line_arguments =
  [
    "-debug",
    Arg.Set debug,
    " display debugging information";

    "-i",
    Arg.Set display_types,
    " display types";

    yes_no
      ~opt:"utf8"
      ~msg:"use UTF-8 for pretty-printing"
      Warp.Print.utf8_output;

    let msg fmt () =
      Format.fprintf fmt
        "automatically scale top-level definitions by %a"
        Warp.Print.pp_omega ()
    in
    yes_no
      ~opt:"auto-const"
      ~msg:(Warp.Print.string_of msg ())
      auto_const;
  ]
