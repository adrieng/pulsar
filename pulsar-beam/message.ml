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

module U = Warp.Utils

type decoding_error = { reason : string; json : Yojson.json; }

exception Could_not_decode of decoding_error

let could_not_decode ~reason ~json () =
  raise (Could_not_decode { reason; json; })

let pos_to_json pos =
  `List [ `Int (Loc.lnum pos); `Int (Loc.cnum pos); ]

let loc_to_json loc =
  let start, stop = Loc.range loc in
  `Assoc [ "file", `String (Loc.file loc);
           "start", pos_to_json start;
           "stop", pos_to_json stop; ]

let pos_of_json json =
  match json with
  | `List [ `Int lnum; `Int cnum; ] ->
     Loc.make_pos ~lnum ~cnum
  | _ ->
     could_not_decode ~json ~reason:"expected [ i, j ]" ()

let loc_of_json json =
  match json with
  | `Assoc [ "file", `String fn; "start", start; "stop", stop; ] ->
     Loc.make ~fn ~start:(pos_of_json start) ~stop:(pos_of_json stop)
  | _ ->
     let reason =
       "expected { \"file\": \"...\", \"start\": pos, \"stop\": pos }"
     in
     could_not_decode ~json ~reason ()

let encode_variant tag json =
  `Assoc [ "tag", `String tag; "value", json; ]

let decode_variant json =
  match json with
  | `Assoc [ "tag", `String tag; "value", value; ] ->
     tag, value
  | _ ->
     let reason = "expected { \"tag\": \"...\", \"value\": ... }" in
     could_not_decode ~json ~reason ()

module Request =
struct
  type t =
    | Show of { loc : Loc.t; kind : [`Type]; }

  let of_json json =
    match decode_variant json with
    | "show", `Assoc [ "loc", loc; "kind", `String "Type"; ] ->
       Show { loc = loc_of_json loc; kind = `Type; }
    | _, json ->
       could_not_decode ~json ~reason:"ill-formed request" ()

  let to_json req =
    let tag, value =
      match req with
      | Show { loc; kind; } ->
         let kind =
           match kind with
           | `Type ->
              "Type"
         in
         "show", `Assoc [ "loc", loc_to_json loc; "kind", `String kind; ]
    in
    encode_variant tag value
end

module Response =
struct
  type ok =
    | Show of { loc : Loc.t; content : string; }

  type ko =
    | Decoding of { reason : string; }

  type t =
    | Ok of ok
    | Ko of ko

  let ok_of_json json =
    match decode_variant json with
    | "show", `Assoc [ "loc", loc; "content", `String content; ] ->
       Show { loc = loc_of_json loc; content; }
    | _, json ->
       could_not_decode ~json ~reason:"ill-formed ok" ()

  let ok_to_json ok =
    let tag, value =
      match ok with
      | Show { loc; content; } ->
         "show",
         `Assoc [ "loc", loc_to_json loc;
                  "content", `String content; ]
    in
    encode_variant tag value

  let ko_of_json json =
    match decode_variant json with
    | "decoding", `String reason ->
       Decoding { reason; }
    | _, json ->
       could_not_decode ~json ~reason:"ill-formed ko" ()

  let ko_to_json ko =
    let tag, value =
      match ko with
      | Decoding { reason; } ->
         "decoding",
         `String reason
    in
    encode_variant tag value

  let of_json json =
    let tag, json = decode_variant json in
    match tag with
    | "ok" ->
       Ok (ok_of_json json)
    | "ko" ->
       Ko (ko_of_json json)
    | _ ->
       let reason = "unknown response tag " ^ tag in
       could_not_decode ~json ~reason ()

  let to_json resp =
    let tag, value =
      match resp with
      | Ko ko ->
         "ko", ko_to_json ko
      | Ok ok ->
         "ok", ok_to_json ok
    in
    encode_variant tag value
end
