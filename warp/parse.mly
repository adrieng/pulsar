(* -*- mode: tuareg -*- *)

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

%{
  let make_extremal_or_pattern prefix ppattern =
    (* This is where we distinguish 0 the extremal pattern from 0 the integer.
       This cannot be done cleanly in the lexer. *)
    if Word.has_null_weight ppattern
    then Periodic.(extremal ~prefix Zero)
    else Periodic.pattern ~prefix ~ppattern ()
%}

%start<Periodic.t> periodic

%token<int> LINT
%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token CARET
%token OMEGA

%token EOF

%%

%inline paren(X):
| LPAREN x = X RPAREN { x }

%inline braced(X):
| LBRACE x = X RBRACE { x }

int:
| i = LINT { i }

singleton_or_brace_tword:
| i = int { Word.singleton i }
| w = braced(nonempty_tword) { w }

nonempty_tword:
| l = nonempty_list(singleton_or_brace_tword)
        { Word.concat l }
| w = singleton_or_brace_tword CARET i = LINT { Word.power w i }

tword:
| w = nonempty_tword { w }
| { Word.empty }

pword:
| u = tword paren(OMEGA)
        { Periodic.extremal ~prefix:u Periodic.Omega }
| u = tword v = paren(nonempty_tword)
        { make_extremal_or_pattern u v }

periodic:
| pword EOF { $1 }
