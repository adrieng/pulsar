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

open Message

module Find = Source_tree_utils.Find(Typed_tree.T)

let diagnoses = ref []

let type_file filename =
  Compiler.Pass.run Flow.frontend filename

let show_type file pos =
  match type_file file with
  | Compiler.Pass.Correct source_tree ->
     begin
       try
         let thing = Find.find_in_file pos source_tree in
         let loc, content =
           let open Typed_tree in
           match thing with
           | `Pat p ->
              p.p_loc, Warp.Print.string_of Type.print p.p_ann
           | `Coe c ->
              c.c_loc, Warp.Print.string_of CoeAnnot.print c.c_ann
           | `Eq eq ->
              eq.eq_loc, Warp.Print.string_of Type.print eq.eq_ann
           | `Exp e ->
              e.e_loc, Warp.Print.string_of Type.print e.e_ann
           | `Phr ph ->
              ph.ph_loc, "phrase"
           | `Block block ->
              block.b_loc, "block"
         in
         Response.Show { loc; content; }
       with Not_found ->
         Response.Silent
     end
  | Compiler.Pass.Error _ ->
     Response.Diagnoses (List.rev !diagnoses)

let diagnosis file =
  Options.diag := Options.Everything;
  match type_file file with
  | Compiler.Pass.Correct _ ->
     Response.Diagnoses (List.rev !diagnoses)
  | Compiler.Pass.Error _ ->
     Response.Diagnoses (List.rev !diagnoses)

let process req =
  match req with
  | Request.Show { file; pos; kind = `Type; } ->
     Response.Ok (show_type file pos)
  | Request.Diagnosis { file; } ->
     Response.Ok (diagnosis file)
