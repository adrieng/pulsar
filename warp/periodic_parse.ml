(* This file is part of Pulsar, a temporal functional language.
 * Copyright (C) 2017 Adrien Guatto
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
n * Foundation, either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the LICENSE file in the top-level directory.
 *)

open Parse

let utf8_string_of_uchar_array a =
  let b = Buffer.create (Array.length a) in
  Array.iter (Buffer.add_utf_8_uchar b) a;
  Buffer.contents b

let string_of_token = function
  | LINT _ -> "INT"
  | OMEGA -> "OMEGA"
  | CARET -> "CARET"
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | LBRACE -> "LBRACE"
  | RBRACE -> "RBRACE"
  | EOF -> "EOF"

let syntax_error reason =
  invalid_arg (Printf.sprintf "syntax error: %s" reason)

let unexpected_token tok =
  syntax_error ("unexpected token " ^ string_of_token tok)

let invalid_character lexbuf =
  let reason =
    match Sedlexing.next lexbuf with
    | None -> "unknown character"
    | Some c ->
       let s = utf8_string_of_uchar_array [| c |] in
       Printf.sprintf "invalid character '%s'" s
  in
  syntax_error reason

let lexeme_string lexbuf =
  utf8_string_of_uchar_array (Sedlexing.lexeme lexbuf)

let lexeme_int lexbuf = lexeme_string lexbuf |> int_of_string

let rec token lexbuf =
  let open Parse in
  match%sedlex lexbuf with
  | white_space -> token lexbuf
  | Plus '0'..'9' -> LINT (lexeme_int lexbuf)
  | '(' -> LPAREN
  | ')' -> RPAREN
  | '^' -> CARET
  | '{' -> LBRACE
  | '}' -> RBRACE
  | 'w' | 0x03C9 -> OMEGA
  | eof -> EOF
  | _ -> invalid_character lexbuf

let of_string s =
  let lexbuf = Sedlexing.Utf8.from_string s in
  let token = Sedlexing.with_tokenizer token lexbuf in
  let file = MenhirLib.Convert.Simplified.traditional2revised Parse.periodic in
  file token
