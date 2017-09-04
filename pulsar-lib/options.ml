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

type pass_info =
  {
    mutable debug : bool;
    mutable serialize : bool;
    mutable stop_after : bool;
    mutable stop_before : bool;
  }

let pass_table : (string, pass_info) Hashtbl.t =
  Hashtbl.create 25

let find_pass s =
  try Hashtbl.find pass_table s
  with Not_found ->
    let pi =
      {
        debug = false;
        serialize = false;
        stop_after = false;
        stop_before = false;
      }
    in
    Hashtbl.add pass_table s pi;
    pi

let set_debug s =
  (find_pass s).debug <- true

let pass_debug s =
  (find_pass s).debug

let set_serialize s =
  (find_pass s).serialize <- true

let pass_serialize s =
  (find_pass s).serialize

let set_stop_before s =
  (find_pass s).stop_before <- true

let pass_stop_before s =
  (find_pass s).stop_before

let set_stop_after s =
  (find_pass s).stop_after <- true

let pass_stop_after s =
  (find_pass s).stop_after

(* Misc global options *)

let display_types = ref false

let auto_const = ref true

let auto_shrink = ref true

let serialize_dir = ref ""

(* Diagnosis *)

type diagnosis_kind =
  | Nothing
  | Everything
  | Buffer

let diag : diagnosis_kind ref =
  ref Nothing

let diagnosis_kinds =
  [
    "nothing";
    "buffers";
    "everything";
  ]

let diagnose kind =
  let about =
    match kind with
    | "nothing" -> Nothing
    | "buffers" -> Buffer
    | "everything" -> Everything
    | _ -> invalid_arg @@ "diagnose: bad argument " ^ kind
  in
  diag := about

(* Command-line arguments related to global options *)

let global_command_line_arguments =
  [
    "-debug",
    Arg.Set debug,
    " display debugging information";

    "-i",
    Arg.Set display_types,
    " display types";

    "-d",
    Arg.Symbol (diagnosis_kinds, diagnose),
    " display additional information about";

    yes_no
      ~opt:"utf8"
      ~msg:"use UTF-8 for pretty-printing"
      Warp.Print.utf8_output;

    yes_no
      ~opt:"auto-const"
      ~msg:(
        let msg fmt () =
          Format.fprintf fmt
                         "automatically scale top-level definitions by %a"
                         Warp.Print.pp_omega ()
        in
        Warp.Print.string_of msg ()
      )
      auto_const;

    yes_no
      ~opt:"auto-shrink"
      ~msg:"automatically shrink implicit coercions"
      auto_shrink;
  ]
