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

module T = Source_tree.Make(
  struct
    module Id =
      struct
        type t = Ident.t
        let print = Ident.print_source
        let compare = Ident.compare
      end

    module PatAnnot = Type
    module ExpAnnot = Type
    module EquAnnot = Type
    module PhrAnnot = Warp.Utils.PrintableOrderedUnit
    module FileAnnot =
      struct
        type t = Type.t Ident.Env.t
        let print = Ident.Env.print Type.print
        let compare _ _ = 0
      end
  end
)

let coerce_with e c ty =
  let open T in
  match c, e.e_desc with
  | Coercion.Id, _ ->
     e
  | _, ESub { ctx; exp; res; } ->
     { e with e_desc = ESub { ctx; exp; res = Coercion.seq (res, c); }; }
  | _ ->
    {
      e_desc = T.ESub { ctx = []; exp = e; res = c; };
      e_loc = e.e_loc;
      e_ann = ty;
    }

let print_interface fmt file =
  let print_binding fmt (k, v) =
    Format.fprintf fmt "@[val %a@ : %a@]"
      Ident.print_source k
      Type.print v
  in
  Format.fprintf fmt "@[<v>%a@]"
    (Warp.Print.pp_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@;@;")
       print_binding)
    (Ident.Env.to_list file)
