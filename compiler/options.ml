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
  ]
