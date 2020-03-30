(* This file is part of Pulsar, a temporal functional language.
 * Copyright (C) 2017-2020 Adrien Guatto
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

module Make(Name : Wdr.ExtSigs.OrderedPrintableHashed0) = struct
  module H = Hashtbl.Make(Name)

  let ht =
    H.create 100

  type t =
    {
      id : int;
      name : Name.t;
    }

  let equal x y =
    x.id = y.id

  let hash x =
    Hashtbl.hash x.id

  let fresh name =
    let ver =
      try H.find ht name
      with Not_found -> let r = ref 0 in H.add ht name r; r
    in
    let id = !ver in
    incr ver;
    { id; name; }

  let name x =
    x.name

  let print { name; id; } =
    PPrint.(Name.print name ^^ underscore ^^ OCaml.int id)
end
