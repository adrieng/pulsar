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

open Typed_tree.T

module Sub = Source_tree_utils.Sub(Typed_tree.T)

let rec diag_buffers_coercion c =
  match c.c_desc with
  | CInvertible _ ->
     ()
  | CSeq (c1, c2) | CArr (c1, c2) | CProd (c1, c2) ->
     diag_buffers_coercion c1;
     diag_buffers_coercion c2
  | CWarped (_, c) ->
     diag_buffers_coercion c
  | CDelay (p, q) ->
     let size = Warp.Formal.size q p in
     let body fmt () =
       Format.fprintf fmt "delay of size %a"
                      Warp.Enat.print size
     in
     Compiler.Diagnostic.info ~loc:c.c_loc ~body ()


let rec diag_buffers thing =
  match thing with
  | `Coe coe ->
     diag_buffers_coercion coe
  | _ ->
     List.iter diag_buffers @@ Sub.sub thing

let pass =
  let diagnosis_file file =
    if !Options.display_types
    then Format.printf "%a@." Typed_tree.print_interface file.f_annot;
    if Options.(!diag = Buffer || !diag = Everything)
    then diag_buffers (`File file);
    file
  in
  Compiler.Pass.atomic
    ~pp_in:Warp.Print.pp_nothing
    ~pp_out:Warp.Print.pp_nothing
    ~name:"diagnosis"
    diagnosis_file
