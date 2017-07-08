type parsing_error =
  | Parsing_error of Loc.loc

let print_parsing_error fmt err =
  match err with
  | Parsing_error loc ->
    Format.fprintf fmt "%a: syntax error (parsing)"
      Loc.print_loc_sameline loc

exception Parsing_error of parsing_error

let parsing_error start stop =
  let loc = Loc.loc_of_lexing_pos_pair start stop in
  raise (Parsing_error (Parsing_error loc))
