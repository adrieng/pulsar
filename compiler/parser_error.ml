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
