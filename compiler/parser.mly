%{
  (* AST helpers *)

  let make_file p phrases =
    {
      Raw_tree.T.name = p.Lexing.pos_fname;
      Raw_tree.T.phrases = phrases;
    }

  let make_def start stop ir x ty e =
    {
      Raw_tree.T.is_rec = ir;
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
    if Warp.Word.has_null_weight ppattern
    then Warp.Periodic.(make_extremal ~prefix Zero)
    else Warp.Periodic.make_pattern ~prefix ~ppattern

  let make_const start stop c =
    make_exp start stop (Raw_tree.T.Const c)

  let make_const_lit start stop l =
    make_const start stop (Const.Lit l)

  let make_by start stop body dr =
    make_exp start stop (Raw_tree.T.By { body; dr; })

  let make_subty start stop ctx_c e c =
    make_exp start stop (Raw_tree.T.Sub (ctx_c, e, c))

  let make_annot start stop e kind ty =
    make_exp start stop (Raw_tree.T.Annot { exp = e; kind; annot = ty; })

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
%token CONS
%token BY
%token LET
%token BOOL
%token CHAR
%token FLOAT
%token INT
%token UNIT
%token STREAM
%token ARR
%token WARR
%token MOD
%token POWER
%token TICK

%token SUBTY
%token LLANGLE
%token RRANGLE
%token LBRACEIMARK
%token RBRACEIMARK

%token ID
%token WRAP
%token UNWRAP
%token CONCAT
%token DECAT
%token DIST
%token FACT
%token INFL
%token DEFL
%token DELAY

%token EOF

(* Priorities *)

%nonassoc SUBTY

%right ARR
%left PLUS MINUS
%left TIMES DIV
%nonassoc MOD
%right CONS

%left SEMICOLON

%left WHERE
%left COLON
%left APP
%left WHEN

%%

(* Utilities *)

%inline paren(X):
| LPAREN x = X RPAREN { x }

(* Periodic warps *)

int:
| i = LINT { i }

singleton_or_brace_tword:
| i = int { Warp.Word.singleton i }
| LBRACE w = nonempty_tword RBRACE { w }

nonempty_tword:
| l = nonempty_list(singleton_or_brace_tword)
        { Warp.Word.concat l }
| w = singleton_or_brace_tword POWER i = LINT { Warp.Word.power w i }

tword:
| w = nonempty_tword { w }
| { Warp.Word.empty }

pword:
| u = tword v = paren(OMEGA)
        { Warp.Periodic.make_extremal ~prefix:u Warp.Periodic.Omega }
| u = tword v = paren(nonempty_tword)
        { make_extremal_or_pattern u v }

warp_ty:
| TICK p = pword { Warp_type.make p }

(* Types *)

bty:
| UNIT { Types.Unit }
| BOOL { Types.Bool }
| CHAR { Types.Char }
| INT { Types.Int }
| FLOAT { Types.Float }

ty:
| bty = bty { Types.Base bty }
| STREAM bty = bty { Types.Stream bty }
| ty1 = ty TIMES ty2 = ty { Types.Prod (ty1, ty2) }
| ty1 = ty ARR ty2 = ty { Types.Fun (ty1, ty2) }
| p = warp_ty MOD ty = ty { Types.Warped (p, ty) }
| ty = paren(ty) { ty }

annot_kind:
| COLON { Source_tree.Typing }
| SUBTY { Source_tree.Subtyping }

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
| CONS { Const.Cons }

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

(* Coercions *)

invertible:
| WRAP { Coercions.Wrap }
| UNWRAP { Coercions.Unwrap }
| CONCAT p = warp_ty q = warp_ty { Coercions.Concat (p, q) }
| DECAT p = warp_ty q = warp_ty { Coercions.Decat (p, q) }
| DIST { Coercions.Dist }
| FACT { Coercions.Fact }
| INFL { Coercions.Infl }
| DEFL { Coercions.Defl }

coercion:
| ID { Coercions.Id }
| c1 = coercion SEMICOLON c2 = coercion { Coercions.Seq (c1, c2) }
| c1 = coercion ARR c2 = coercion { Coercions.Arr (c1, c2) }
| c1 = coercion TIMES c2 = coercion { Coercions.Prod (c1, c2) }
| p = warp_ty MOD c = coercion { Coercions.Warped (p, c) }
| i = invertible { Coercions.Invertible i }
| DELAY p = warp_ty q = warp_ty { Coercions.Delay (p, q) }
| c = paren(coercion) { c }

ident_coercion:
| id = IDENT LLANGLE c = coercion { (id, c) }

coercion_ctx:
| l = separated_list(COMMA, paren(ident_coercion)) { l }

(* Definitions *)

def:
| LET ir = boption(REC) id = IDENT COLON ty = ty EQUAL e = exp
    { make_def $startpos $endpos ir id ty e }

local_defs:
| LBRACE l = separated_list(SEMICOLON, def) RBRACE { l }

(* Expressions *)

simple_exp:
| id = IDENT
    { make_var $startpos $endpos id }
| l = lit
    { make_const_lit $startpos $endpos l }
| e = paren(exp)
    { e }

exp:
| e = simple_exp
    { e }
| LAM id = IDENT WARR e = exp
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
| e = exp WHERE ir = boption(REC) ld = local_defs
    { make_where $startpos $endpos e ir ld }
| c = const_exp(paren(const))
    { c }
| e = simple_exp BY dr = warp_ty
    { make_by $startpos $endpos e dr }
| LBRACEIMARK
    ctx_c = coercion_ctx
    RRANGLE e = exp
    RRANGLE c = coercion
  RBRACEIMARK
    { make_subty $startpos $endpos ctx_c e c }
| e = exp kind = annot_kind ty = ty %prec SUBTY
    { make_annot $startpos $endpos e kind ty }

phrase:
| d = def { Raw_tree.T.Def d }

file:
| body = list(phrase) EOF { make_file $startpos body }
| error { Parser_error.parsing_error $startpos $endpos }
