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
  | VAL
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

(** Possible lexing errors *)
type lexing_error

(** Pretty-print a lexing error *)
val print_lexing_error : Format.formatter -> lexing_error -> unit

(** Lexing exceptions *)
exception Lexing_error of lexing_error

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
