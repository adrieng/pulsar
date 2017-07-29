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

let test =
  let pos = Loc.make_pos ~lnum:0 ~cnum:0 in
  let loc = Loc.(make ~fn:"test.pul" ~start:pos ~stop:pos) in
  Request.(Show { loc; kind = `Type; })

let _ =
  Yojson.to_channel stdout (Request.to_json test);
  print_newline ();
  ()
