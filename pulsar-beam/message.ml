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

type json = Yojson.Basic.t

type decoding_error = { reason : string; json : json; }

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
    | Show of { file : string; pos : Loc.pos; kind : [`Type]; }
    | Diagnosis of { file : string; }
    | Passes

  let of_json json =
    match decode_variant json with
    | "show", `Assoc [ "file", `String file;
                       "pos", pos;
                       "kind", `String "Type"; ] ->
       Show { file; pos = pos_of_json pos; kind = `Type; }
    | "diagnosis", `Assoc [ "file", `String file; ] ->
       Diagnosis { file; }
    | "passes", _ ->
       Passes
    | _, json ->
       could_not_decode ~json ~reason:"ill-formed request" ()

  let to_json req =
    let tag, value =
      match req with
      | Show { pos; kind; _ } ->
         let kind =
           match kind with
           | `Type ->
              "Type"
         in
         "show", `Assoc [ "pos", pos_to_json pos; "kind", `String kind; ]
      | Diagnosis { file; } ->
         "diagnosis", `Assoc [ "file", `String file; ]
      | Passes ->
         "passes", `Assoc []
    in
    encode_variant tag value
end

module Response =
struct
  type ok =
    | Silent
    | Show of { loc : Loc.t; content : string; }
    | Diagnoses of Compiler.Diagnostic.t list
    | Passes of string list

  type ko =
    | Decoding of { reason : string; }

  type t =
    | Ok of ok
    | Ko of ko

  let diagnostic_of_json json =
    match json with
    | `Assoc [ "loc", loc;
               "pass", `String pass;
               "kind", `String kind;
               "body", `String body; ] ->
       let open Compiler.Diagnostic in
       let kind =
         match kind with
         | "Error" ->
            Error
         | "Warning" ->
            Warning
         | "Info" ->
            Info
         | _ ->
            could_not_decode ~json ~reason:"ill-formed json" ()
       in
       let body fmt () = Format.fprintf fmt "%s" body in
       { loc = loc_of_json loc; pass; kind; body; }
    | _ ->
       could_not_decode ~json ~reason:"ill-formed diagnostic" ()

  let json_of_diagnostic =
    let open Compiler.Diagnostic in
    fun { loc; pass; kind; body; } ->
    let kind =
      match kind with
      | Error -> "Error"
      | Warning -> "Warning"
      | Info -> "Info"
    in
    let body = Warp.Print.string_of body () in
    `Assoc [ "loc", loc_to_json loc;
             "pass", `String pass;
             "kind", `String kind;
             "body", `String body; ]

  let string_of_json js =
    match js with
    | `String s ->
       s
    | _ ->
       failwith "string_of_json: not a string"

  let ok_of_json json =
    match decode_variant json with
    | "silent", `Assoc [] ->
       Silent

    | "show", `Assoc [ "loc", loc;
                       "content", `String content; ] ->
       Show { loc = loc_of_json loc; content; }

    | "diagnoses", `List jsons ->
       Diagnoses (List.map diagnostic_of_json jsons)

    | "passes", `List jsons ->
       Passes (List.map string_of_json jsons)

    | _, json ->
       could_not_decode ~json ~reason:"ill-formed ok" ()

  let ok_to_json ok : json =
    let tag, (value : json) =
      match ok with
      | Silent ->
         "silent",
         `Assoc []

      | Show { loc; content; } ->
         "show",
         `Assoc [ "loc", loc_to_json loc;
                  "content", `String content; ]

      | Diagnoses diags ->
         "diagnoses", `List (List.map json_of_diagnostic diags)

      | Passes passes ->
         "passes", `List (List.map (fun s -> `String s) passes)
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
