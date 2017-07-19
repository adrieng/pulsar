let yes_no r s =
  match s with
  | "yes" ->
    r := true
  | "no" ->
    r := false
  | _ ->
    invalid_arg ("yes_no: yes or no expected, got " ^ s)

let pp_utf8 = ref false

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

let debug_options =
  List.map fst debug_table

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
