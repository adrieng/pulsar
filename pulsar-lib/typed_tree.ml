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

let seq_ctx ctx ctx' =
  let ctx = Ident.Env.of_list ctx in
  let ctx' = Ident.Env.of_list ctx' in
  let seq id c c' =
    match c, c' with
    | None, None -> None
    | Some c, None | None, Some c -> Some c
    | Some c, Some c' -> Some (Coercion.seq (c, c'))
  in
  Ident.Env.to_list @@ Ident.Env.merge seq ctx ctx'

let simplify_ctx ctx =
  seq_ctx ctx []

let coerce_with ?(ctx' = []) ?(res' = Coercion.Id) e ty =
  let open T in
  let ctx' = simplify_ctx ctx' in
  match ctx', res', e.e_desc with
  | [], Coercion.Id, _ ->
     e
  | _, _, ESub { ctx; exp; res; } ->
     let ctx = seq_ctx ctx ctx' in
     let res = Coercion.seq (res, res') in
     if ctx <> [] || res <> Coercion.Id
     then { e with e_desc = ESub { ctx; exp; res; }; }
     else exp
  | _ ->
    {
      e_desc = T.ESub { ctx = ctx'; exp = e; res = res'; };
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
