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

(* Tokens *)

type token =
  (* Var and idents *)
  | IDENT of string
  | MODN of string
  (* Lambda *)
  | LAM
  | ARR
  | WARR
  (* Pair *)
  | LPAREN
  | RPAREN
  | COMMA
  (* Local declarations *)
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
  (* Scalar literals *)
  | LBOOL of bool
  | LCHAR of char
  | LINT of int
  | LFLOAT of float
  (* Extremal pattern (w) *)
  | OMEGA
  (* Other constants *)
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | WHEN
  | MERGE
  | CONS
  (* Warp *)
  | BY
  (* Phrases, declarations *)
  | LET
  | IN
  | EXTERN
  (* Base types *)
  | BOOL
  | CHAR
  | FLOAT
  | INT
  (* Types *)
  | UNIT
  | STREAM
  | MOD
  | POWER
  | TICK
  (* Misc *)
  | SUBTY
  | LLANGLE
  | RRANGLE
  | LBRACEIMARK
  | RBRACEIMARK
  (* Coercions *)
  | ID
  | WRAP | UNWRAP
  | CONCAT | DECAT
  | DIST | FACT
  | INFL | DEFL
  | DELAY
  (* End-of-file, last token *)
  | EOF

let print_token fmt tok =
  match tok with
  | IDENT s ->
    Format.fprintf fmt "IDENT [%s]" s
  | MODN s ->
    Format.fprintf fmt "MODN [%s]" s
  | LAM ->
    Format.fprintf fmt "LAM (%a)"
      Warp.Print.pp_lambda ()
  | ARR ->
    Format.fprintf fmt "ARR (%a)"
      Warp.Print.pp_arrow ()
  | WARR ->
    Format.fprintf fmt "WARR (%a)"
      Warp.Print.pp_thick_arrow ()
  | LPAREN ->
    Format.fprintf fmt "LPAREN"
  | RPAREN ->
    Format.fprintf fmt "RPAREN"
  | COMMA ->
    Format.fprintf fmt "COMMA"
  | WHERE ->
    Format.fprintf fmt "WHERE"
  | REC ->
    Format.fprintf fmt "REC"
  | SEQ ->
    Format.fprintf fmt "SEQ"
  | PAR ->
    Format.fprintf fmt "PAR"
  | LBRACE ->
    Format.fprintf fmt "LBRACE"
  | RBRACE ->
    Format.fprintf fmt "RBRACE"
  | DOT ->
    Format.fprintf fmt "DOT"
  | COLON ->
    Format.fprintf fmt "COLON"
  | EQUAL ->
    Format.fprintf fmt "EQUAL"
  | SEMICOLON ->
    Format.fprintf fmt "SEMICOLON"
  | LBOOL b ->
    Format.fprintf fmt "LBOOL [%b]" b
  | LCHAR c ->
    Format.fprintf fmt "LCHAR [%c]" c
  | OMEGA ->
    Format.fprintf fmt "OMEGA"
  | LINT i ->
    Format.fprintf fmt "LINT [%i]" i
  | LFLOAT f ->
    Format.fprintf fmt "LFLOAT [%f]" f
  | PLUS ->
    Format.fprintf fmt "PLUS"
  | MINUS ->
    Format.fprintf fmt "MINUS"
  | TIMES ->
    Format.fprintf fmt "TIMES (%a)"
      Warp.Print.pp_times ()
  | DIV ->
    Format.fprintf fmt "DIV"
  | WHEN ->
    Format.fprintf fmt "WHEN"
  | MERGE ->
    Format.fprintf fmt "MERGE"
  | CONS ->
    Format.fprintf fmt "CONS"
  | BY ->
    Format.fprintf fmt "BY"
  | LET ->
    Format.fprintf fmt "LET"
  | IN ->
    Format.fprintf fmt "IN"
  | EXTERN ->
    Format.fprintf fmt "EXTERN"
  | MOD ->
    Format.fprintf fmt "MOD (%a)"
      Warp.Print.pp_circledast ()
  | BOOL ->
    Format.fprintf fmt "BOOL"
  | CHAR ->
    Format.fprintf fmt "CHAR"
  | INT ->
    Format.fprintf fmt "INT"
  | FLOAT ->
    Format.fprintf fmt "FLOAT"
  | UNIT ->
    Format.fprintf fmt "UNIT"
  | STREAM ->
    Format.fprintf fmt "STREAM"
  | POWER ->
     Format.fprintf fmt "POWER"
  | TICK ->
     Format.fprintf fmt "TICK"
  | SUBTY ->
     Format.fprintf fmt "SUBTY"
  | LLANGLE ->
     Format.fprintf fmt "LLANGLE"
  | RRANGLE ->
     Format.fprintf fmt "RRANGLE"
  | LBRACEIMARK ->
     Format.fprintf fmt "LBRACEIMARK"
  | RBRACEIMARK ->
     Format.fprintf fmt "RBRACEIMARK"
  | ID ->
     Format.fprintf fmt "ID"
  | WRAP ->
     Format.fprintf fmt "WRAP"
  | UNWRAP ->
     Format.fprintf fmt "UNWRAP"
  | CONCAT ->
     Format.fprintf fmt "CONCAT"
  | DECAT ->
     Format.fprintf fmt "DECAT"
  | DIST ->
     Format.fprintf fmt "DIST"
  | FACT ->
     Format.fprintf fmt "FACT"
  | INFL ->
     Format.fprintf fmt "INFL"
  | DEFL ->
     Format.fprintf fmt "DEFL"
  | DELAY ->
     Format.fprintf fmt "DELAY"
  | EOF ->
    Format.fprintf fmt "EOF"

(* Contexts *)

type ctx =
    {
      filename : string;
      lexbuf : Sedlexing.lexbuf;
      mutable lnum : int; (* current line number *)
      mutable loffset : int; (* offset of the current line measured from the
                                beginning of the file *)
    }

let ctx_of_lexbuf filename lexbuf =
  { filename; lexbuf; lnum = 0; loffset = 0; }

let register_new_line ctx =
  ctx.lnum <- ctx.lnum + 1;
  ctx.loffset <- Sedlexing.lexeme_end ctx.lexbuf

let current_token_start { filename; lexbuf; lnum; loffset; } =
  let open Lexing in
  {
    pos_fname = filename;
    pos_lnum = lnum;
    pos_bol = loffset;
    pos_cnum = Sedlexing.lexeme_start lexbuf;
  }

let current_token_end { filename; lexbuf; lnum; loffset; } =
  let open Lexing in
  {
    pos_fname = filename;
    pos_lnum = lnum;
    pos_bol = loffset;
    pos_cnum = Sedlexing.lexeme_end lexbuf;
  }

let loc_of_last_lexeme ctx =
  let start = Loc.pos_of_lexing_pos (current_token_start ctx) in
  let stop = Loc.pos_of_lexing_pos (current_token_end ctx) in
  Loc.make_loc ~fn:ctx.filename ~start ~stop

let ctx_from_utf8_channel ~filename ic =
  ctx_of_lexbuf filename (Sedlexing.Utf8.from_channel ic)

let lexeme_uarray ctx =
  Sedlexing.lexeme ctx.lexbuf

let lexeme_ascii ctx =
  let a = lexeme_uarray ctx in
  let n = Array.length a in
  let s = Bytes.make n '?' in
  for i = 0 to n - 1 do
    if not (Warp.Utils.is_ascii a.(i)) then failwith "non-ascii character";
    Bytes.set s i (Char.chr a.(i))
  done;
  Bytes.to_string s

(* Errors *)

let bad_token ctx =
  let text = "syntax error (lexing)" in
  Compiler.Message.error ~loc:(loc_of_last_lexeme ctx) ~text ()

let unterminated_comment ctx =
  let text = "unterminated comment" in
  Compiler.Message.error ~loc:(loc_of_last_lexeme ctx) ~text ()

(* Lexing itself *)

let lexeme_bool ctx =
  bool_of_string (lexeme_ascii ctx)

let lexeme_char ctx =
  let a = lexeme_uarray ctx in
  assert (Array.length a = 3);
  assert (a.(0) = Char.code '\'');
  assert (a.(2) = Char.code '\'');
  if not (Warp.Utils.is_ascii a.(1)) then bad_token ctx;
  Char.chr a.(1)

let lexeme_int ctx =
  int_of_string (lexeme_ascii ctx)

let lexeme_float ctx =
  float_of_string (lexeme_ascii ctx)

let find_keyword =
  let keyword_table =
    Warp.Utils.hashtable_of_assoc_list
      [
        "where", WHERE;
        "rec", REC;
        "seq", SEQ;
        "par", PAR;
        "when", WHEN;
        "merge", MERGE;
        "by", BY;
        "let", LET;
        "in", IN;
        "extern", EXTERN;
        "bool", BOOL;
        "char", CHAR;
        "float", FLOAT;
        "int", INT;
        "unit", UNIT;
        "stream", STREAM;
        "id", ID;
        "wrap", WRAP;
        "unwrap", UNWRAP;
        "concat", CONCAT;
        "decat", DECAT;
        "dist", DIST;
        "fact", FACT;
        "infl", INFL;
        "defl", DEFL;
        "delay", DELAY;
      ]
  in
  Hashtbl.find keyword_table

let keyword_or_ident s =
  try find_keyword s
  with Not_found -> IDENT s

let op =
  [%sedlex.regexp? Plus (Chars "+*-/")]

let white_space_no_newline =
  [%sedlex.regexp? " " | "\t"]

let new_line =
  [%sedlex.regexp? "\n"]

let ascii_digit =
  [%sedlex.regexp? '0'..'9']

let id =
  [%sedlex.regexp?
      'a'..'z', Star ('a'..'z' | 'A'..'Z' | ascii_digit | "'" | "_")]

let modn =
  [%sedlex.regexp?
      'A'..'Z', Star ('a'..'z' | 'A'..'Z' | ascii_digit | "_")]

let bool =
  [%sedlex.regexp? "true" | "false"]

let char =
  [%sedlex.regexp? "'", any, "'"]

let optsign =
  [%sedlex.regexp? "" | "-" | "+"]

let int =
  [%sedlex.regexp? optsign, Plus '0'..'9']

let exponent =
  [%sedlex.regexp? ('e' | 'E'), optsign, Plus ascii_digit]

let float_body =
  [%sedlex.regexp?
    Plus ascii_digit, ".", Star ascii_digit, Opt exponent
  | Star ascii_digit, ".", Plus ascii_digit, Opt exponent
  | Plus ascii_digit, exponent]

let float =
  [%sedlex.regexp? optsign, float_body]

let rec next_token ctx =
  let lexbuf = ctx.lexbuf in

  let rec skip_comments unterminated =
    if unterminated > 0 then
      match%sedlex lexbuf with
      | "(*" ->
        skip_comments (unterminated + 1)
      | "*)" ->
        skip_comments (unterminated - 1)
      | new_line ->
        register_new_line ctx;
        skip_comments unterminated
      | eof ->
        unterminated_comment ctx
      | any ->
        skip_comments unterminated
      | _ ->
        bad_token ctx
  in

  match%sedlex lexbuf with
  | "(*" ->
    skip_comments 1;
    next_token ctx

  | bool ->
    LBOOL (lexeme_bool ctx)
  | char ->
    LCHAR (lexeme_char ctx)
  | int ->
    LINT (lexeme_int ctx)
  | float ->
    LFLOAT (lexeme_float ctx)

  | "\\w" | 0x03C9 -> OMEGA

  | "<<" -> LLANGLE
  | ">>" -> RRANGLE
  | "<:" -> SUBTY

  | "{!" -> LBRACEIMARK
  | "!}" -> RBRACEIMARK

  | "\\" | 0x03BB -> LAM
  | "->" | 0x2192 -> ARR
  | "=>" | 0x21D2 -> WARR
  | "(" -> LPAREN
  | ")" -> RPAREN
  | "," -> COMMA
  | "{" -> LBRACE
  | "}" -> RBRACE
  | "::" -> CONS
  | "." -> DOT
  | ":" -> COLON
  | "=" -> EQUAL
  | ";" -> SEMICOLON
  | "<*>" | 0x229B -> MOD
  | "^" -> POWER

  | "+" -> PLUS
  | "-" -> MINUS
  | "*" | 0x00D7 -> TIMES
  | "/" -> DIV
  | "`" -> TICK

  | id -> keyword_or_ident (lexeme_ascii ctx)
  | modn -> MODN (lexeme_ascii ctx)

  | white_space_no_newline -> next_token ctx
  | new_line -> register_new_line ctx; next_token ctx

  | eof -> EOF
  | _ -> bad_token ctx

let next_token_pos ctx =
  let tok = next_token ctx in
  tok, current_token_start ctx, current_token_end ctx
