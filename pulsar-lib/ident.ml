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

type kind = Source | Internal

type t =
  {
    num : int;
    name : string;
    name_num : int;
    kind : kind;
  }

type ctx =
  {
    mutable global_cpt : int;
    available_name_nums : (string, int ref) Hashtbl.t;
  }

let make_ctx () =
  {
    global_cpt = 0;
    available_name_nums = Hashtbl.create 1000;
  }

let current_ctx = ref (make_ctx ())

let get_current_ctx () = !current_ctx

let set_current_ctx ctx = current_ctx := ctx

let reset_ctx () = set_current_ctx (make_ctx ())

let make_ident kind name =
  let available_name_num =
    try Hashtbl.find !current_ctx.available_name_nums name
    with Not_found ->
      let r = ref 0 in
      Hashtbl.add !current_ctx.available_name_nums name r;
      r
  in
  let id =
    {
      num = !current_ctx.global_cpt;
      name = name;
      name_num = !available_name_num;
      kind = kind;
    }
  in
  !current_ctx.global_cpt <- !current_ctx.global_cpt + 1;
  incr available_name_num;
  id

let compare b1 b2 = b2.num - b1.num

let make_source = make_ident Source
let make_internal = make_ident Internal

let make_suffix b s = make_internal (b.name ^ s)

let make_prefix s b = make_internal (s ^ b.name)

let refresh id = make_ident id.kind id.name

let hash id = Hashtbl.hash id.num

let equal id1 id2 = 0 = compare id1 id2

type t_ = t
module Ident_ordered =
struct
  type t = t_
  let compare = compare
end

let to_string id =
  let print_short = id.kind = Source && id.name_num = 0 in
  if print_short then id.name else Printf.sprintf "%s_%d" id.name id.name_num

let print fmt id = Format.fprintf fmt "%s" (to_string id)

let print_source fmt id =
  let repr =
    if id.kind = Source && not !Options.debug then id.name else to_string id
  in
  Format.fprintf fmt "%s" repr

module Set =
struct
  module M = Set.Make(Ident_ordered)

  include M

  let print fmt s =
    let l = M.fold (fun x l -> x :: l) s [] in
    Warp.Print.(pp_list
                  ~pp_sep:pp_comma
                  print) fmt l

  let unions ss =
    let rec loop acc ss =
      match ss with
      | [] ->
         acc
      | s :: ss ->
         loop (union acc s) ss
    in
    loop empty ss
end

module Env =
struct
  module M = Warp.Utils.MakeMap(Ident_ordered)

  include M

  let trim env set =
    let add k v env = if Set.mem k set then M.add k v env else env in
    M.fold add env M.empty

  let mapfold f env acc =
    let add k v (env, acc) =
      let (k, v), acc = f (k, v) acc in
      M.add k v env, acc
    in
    M.fold add env (M.empty, acc)

  let mapfold_elems f env acc =
    let add k v (env, acc) =
      let v, acc = f v acc in
      M.add k v env, acc
    in
    M.fold add env (M.empty, acc)

  let cardinal env = M.fold (fun _ _ n -> n + 1) env 0

  let print ?(sep = ",") print_elt fmt env =
    print ~sep print_source print_elt fmt env
end
