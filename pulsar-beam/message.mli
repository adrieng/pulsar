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

type decoding_error = { reason : string; json : Yojson.json; }

exception Could_not_decode of decoding_error

module Request :
sig
  type t =
    | Show of { loc : Loc.t; kind : [`Type]; }

  val of_json : Yojson.json -> t

  val to_json : t -> Yojson.json
end

module Response :
sig
  type ok =
    | Show of { loc : Loc.t; content : string; }

  type ko =
    | Decoding of { reason : string; }

  type t =
    | Ok of ok
    | Ko of ko

  val of_json : Yojson.json -> t

  val to_json : t -> Yojson.json
end
