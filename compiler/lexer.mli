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

(** Lexemes *)
type token =
  | IDENT of string
  | MODN of string
  | LAM
  | ARR
  | WARR
  | LPAREN
  | RPAREN
  | COMMA
  | WHERE
  | REC
  | SEQ
  | PAR
  | LBRACE
  | RBRACE
  | DOT
  | COLON
  | EQUAL
  | SEMICOLON
  | LBOOL of bool
  | LCHAR of char
  | LINT of int
  | LFLOAT of float
  | OMEGA
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | WHEN
  | MERGE
  | CONS
  | BY
  | LET
  | IN
  | EXTERN
  | BOOL
  | CHAR
  | FLOAT
  | INT
  | UNIT
  | STREAM
  | MOD
  | POWER
  | TICK
  | SUBTY
  | LLANGLE
  | RRANGLE
  | LBRACEIMARK
  | RBRACEIMARK
  | ID
  | WRAP
  | UNWRAP
  | CONCAT
  | DECAT
  | DIST
  | FACT
  | INFL
  | DEFL
  | DELAY
  | EOF

(** Pretty-print a token; for debugging purposes *)
val print_token : Format.formatter -> token -> unit

(** Lexing contexts *)
type ctx

(** Turn an UTF-8 channel with name [filename] into a lexing context *)
val ctx_from_utf8_channel : filename:string -> in_channel -> ctx

(** Get the start position of the current token *)
val current_token_start : ctx -> Lexing.position

(** Get the end position of the current token *)
val current_token_end : ctx -> Lexing.position

(** Get the next token *)
val next_token : ctx -> token

(** Get the next token and its start/end positions *)
val next_token_pos : ctx -> token * Lexing.position * Lexing.position
