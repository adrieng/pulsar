%{
  (* AST helpers *)

  let make_file p phrases =
    {
      Raw_tree.T.name = p.Lexing.pos_fname;
      Raw_tree.T.phrases = phrases;
    }

  let make_def start stop x ty e =
    {
      Raw_tree.T.lhs = x;
      Raw_tree.T.tydf = ty;
      Raw_tree.T.rhs = e;
      Raw_tree.T.locdf = Loc.loc_of_lexing_pos_pair ~start ~stop;
    }

  let make_decl start stop x ty =
    {
      Raw_tree.T.name = x;
      Raw_tree.T.tydl = ty;
      Raw_tree.T.locdl = Loc.loc_of_lexing_pos_pair ~start ~stop;
    }

  let make_exp start stop desc =
    {
      Raw_tree.T.desc = desc;
      Raw_tree.T.loc = Loc.loc_of_lexing_pos_pair ~start ~stop;
      Raw_tree.T.ann = ();
    }

  let make_var start stop x =
    make_exp start stop (Raw_tree.T.Var x)

  let make_lam start stop x e =
    make_exp start stop (Raw_tree.T.Lam (x, e))

  let make_pair start stop e1 e2 =
    make_exp start stop (Raw_tree.T.Pair (e1, e2))

  let make_fst start stop e =
    make_exp start stop (Raw_tree.T.Fst e)

  let make_snd start stop e =
    make_exp start stop (Raw_tree.T.Snd e)

  let make_where start stop body is_rec defs =
    make_exp start stop (Raw_tree.T.Where { body; is_rec; defs; })

  let make_extremal_or_pattern prefix ppattern =
    (* This is where we distinguish 0 the extremal pattern from 0 the integer.
       This cannot be done cleanly in the lexer. *)
    if Clock.Word.has_null_weight ppattern
    then Clock.Periodic.(make_extremal ~prefix Zero)
    else Clock.Periodic.make_pattern ~prefix ~ppattern

  let make_const start stop c =
    make_exp start stop (Raw_tree.T.Const c)

  let make_const_lit start stop l =
    make_const start stop (Const.Lit l)

  let make_shift start stop e ck ty =
    make_exp start stop (Raw_tree.T.Shift (e, ck, ty))

  let make_scale start stop body dr locals =
    make_exp start stop (Raw_tree.T.Scale { body; dr; locals; })

  let make_annot start stop e ty =
    make_exp start stop (Raw_tree.T.Annot (e, ty))

  let make_op start stop op =
    make_exp start stop (Raw_tree.T.Const (Const.Op op))

  let make_binop_app e1 e2 e3 =
    Raw_tree.(make_app (make_app e1 e2) e3)
%}

%start<Raw_tree.T.file> file

(* Tokens; should be the same as in lexer.mli *)

%token<string> IDENT
%token LAM
%token LPAREN
%token RPAREN
%token COMMA
%token WHERE
%token REC
%token LBRACE
%token RBRACE
%token COLON
%token EQUAL
%token SEMICOLON
%token<bool> LBOOL
%token<char> LCHAR
%token<int> LINT
%token<float> LFLOAT
%token OMEGA
%token PLUS
%token MINUS
%token TIMES
%token DIV
%token WHEN
%token MERGE
%token SHIFT
%token TO
%token SCALE
%token BY
%token WITH
%token LET
%token BOOL
%token CHAR
%token FLOAT
%token INT
%token UNIT
%token STREAM
%token ARR
%token MOD
%token POWER
%token EOF

(* Priorities *)

%right ARR
%left PLUS MINUS
%left TIMES DIV
%nonassoc MOD

%left WHERE
%left COLON
%left APP
%left WHEN

%%

(* Utilities *)

%inline paren(X):
| LPAREN x = X RPAREN { x }

(* Periodic clocks *)

semicolon_nonempty_list(X):
| x = X { [x] }
| x = X SEMICOLON { [x] }
| x = X SEMICOLON l = semicolon_nonempty_list(X) { x :: l }

int:
| i = LINT { i }

singleton_or_brace_tword:
| i = int { Clock.Word.singleton i }
| LBRACE w = nonempty_tword RBRACE { w }

nonempty_tword:
| l = nonempty_list(singleton_or_brace_tword) { Clock.Word.concat l }
| w = singleton_or_brace_tword POWER i = LINT { Clock.Word.power w i }

tword:
| w = nonempty_tword { w }
| { Clock.Word.empty }

pword:
| u = tword LPAREN v = OMEGA RPAREN { Clock.Periodic.make_extremal ~prefix:u Clock.Periodic.Omega }
| u = tword LPAREN v = nonempty_tword RPAREN { make_extremal_or_pattern u v }

clock_ty:
| p = pword { Clock_type.make p }

(* Types *)

bty:
| BOOL { Types.Bool }
| CHAR { Types.Char }
| INT { Types.Int }
| FLOAT { Types.Float }

ty:
| UNIT { Types.Unit }
| STREAM bty = bty { Types.Stream bty }
| ty1 = ty TIMES ty2 = ty { Types.Prod (ty1, ty2) }
| ty1 = ty ARR ty2 = ty { Types.Fun (ty1, ty2) }
| MOD ck = clock_ty ty = ty { Types.Box (ck, ty) }
| LPAREN ty = ty RPAREN { ty }

(* Literals, operators, and constants *)

%inline const_exp(C):
| c = C { make_const $startpos $endpos c }

lit:
| b = LBOOL { Scal.Bool b }
| c = LCHAR { Scal.Char c }
| i = LINT { Scal.Int i }
| f = LFLOAT { Scal.Float f }

%inline op:
| PLUS { Const.Plus }
| MINUS { Const.Minus }
| TIMES { Const.Times }
| DIV { Const.Div }

%inline cwhen:
| WHEN p = pword { Const.When p }

%inline cmerge:
| MERGE p = pword { Const.Merge p }

%inline cop:
| o = op { Const.Op o }

%inline eop:
| o = const_exp(cop) { o }

%inline const:
| c = cop { c }
| c = cwhen { c }
| c = cmerge { c }

(* Definitions and declarations *)

def:
| id = IDENT COLON ty = ty EQUAL e = exp
    { make_def $startpos $endpos id ty e }

decl:
| id = IDENT COLON ty = ty { make_decl $startpos $endpos id ty }

local_defs:
| l = semicolon_nonempty_list(def) { l }

local_decls:
| l = semicolon_nonempty_list(decl) { l }

(* Expressions *)

simple_exp:
| id = IDENT
    { make_var $startpos $endpos id }
| l = lit
    { make_const_lit $startpos $endpos l }
| LPAREN e = exp RPAREN
    { e }

exp:
| e = simple_exp
    { e }
| LAM id = IDENT ARR e = exp
    { make_lam $startpos $endpos id e }
| e1 = simple_exp e2 = exp %prec APP
    { Raw_tree.make_app e1 e2 }

| e1 = exp op = eop e2 = exp
    { make_binop_app op e1 e2 }
| e1 = exp e2 = const_exp(cwhen)
    { Raw_tree.make_app e2 e1 }
| e1 = const_exp(cmerge) e2 = simple_exp e3 = simple_exp
    { make_binop_app e1 e2 e3 }

| LPAREN e1 = exp COMMA e2 = exp RPAREN
    { make_pair $startpos $endpos e1 e2 }
| e = exp WHERE ir = boption(REC) LBRACE ld = local_defs RBRACE
    { make_where $startpos $endpos e ir ld }
| c = const_exp(paren(const))
    { c }
| SHIFT e = exp TO MOD ck = clock_ty ty = ty
    { make_shift $startpos $endpos e ck ty }
| SCALE e = exp BY dr = clock_ty WITH LBRACE locals = local_decls RBRACE
    { make_scale $startpos $endpos e dr locals }
| e = exp COLON ty = ty
    { make_annot $startpos $endpos e ty }

phrase:
| LET d = def { Raw_tree.T.Def d }

file:
| body = list(phrase) EOF { make_file $startpos body }
| error { Parser_error.parsing_error $startpos $endpos }
