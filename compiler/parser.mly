%{
  (* AST helpers *)

  let make_file p phrases =
    {
      Raw_tree.T.f_name = p.Lexing.pos_fname;
      Raw_tree.T.f_phrases = phrases;
    }

  let make_decl start stop id ty =
    {
      Raw_tree.T.ph_desc = Raw_tree.T.PDecl { id; ty; };
      Raw_tree.T.ph_loc = Loc.loc_of_lexing_pos_pair ~start ~stop;
      Raw_tree.T.ph_ann = ();
    }

  let make_def start stop block =
    {
      Raw_tree.T.ph_desc = Raw_tree.T.PDef block;
      Raw_tree.T.ph_loc = Loc.loc_of_lexing_pos_pair ~start ~stop;
      Raw_tree.T.ph_ann = ();
    }

  let make_pat start stop desc : Raw_tree.T.pat =
    {
      Raw_tree.T.p_desc = desc;
      Raw_tree.T.p_loc = Loc.loc_of_lexing_pos_pair ~start ~stop;
      Raw_tree.T.p_ann = ();
    }

  let make_eq start stop p params res_ty e =
    {
      Raw_tree.T.eq_lhs = p;
      Raw_tree.T.eq_params = params;
      Raw_tree.T.eq_ty = res_ty;
      Raw_tree.T.eq_rhs = e;
      Raw_tree.T.eq_loc = Loc.loc_of_lexing_pos_pair ~start ~stop;
      Raw_tree.T.eq_ann = ();
    }

  let make_exp start stop desc =
    {
      Raw_tree.T.e_desc = desc;
      Raw_tree.T.e_loc = Loc.loc_of_lexing_pos_pair ~start ~stop;
      Raw_tree.T.e_ann = ();
    }

  let make_var start stop x =
    make_exp start stop (Raw_tree.T.EVar x)

  let make_external start stop ln =
    make_exp start stop (Raw_tree.T.EExternal ln)

  let make_lam start stop (p : Raw_tree.T.pat) e =
    make_exp start stop (Raw_tree.T.ELam (p, e))

  let make_cons start stop e1 e2 =
    make_exp start stop (Raw_tree.T.ECons (e1, e2))

  let make_pair start stop e1 e2 =
    make_exp start stop (Raw_tree.T.EPair (e1, e2))

  let make_fst start stop e =
    make_exp start stop (Raw_tree.T.EFst e)

  let make_snd start stop e =
    make_exp start stop (Raw_tree.T.ESnd e)

  let make_block start stop b_rec b_body =
    {
      Raw_tree.T.b_rec;
      Raw_tree.T.b_body;
      Raw_tree.T.b_loc = Loc.loc_of_lexing_pos_pair start stop;
    }

  let make_where start stop body block =
    make_exp start stop (Raw_tree.T.EWhere { body; block; })

  let make_extremal_or_pattern prefix ppattern =
    (* This is where we distinguish 0 the extremal pattern from 0 the integer.
       This cannot be done cleanly in the lexer. *)
    if Warp.Word.has_null_weight ppattern
    then Warp.Periodic.(make_extremal ~prefix Zero)
    else Warp.Periodic.make_pattern ~prefix ~ppattern

  let make_const start stop c =
    make_exp start stop (Raw_tree.T.EConst c)

  let make_const_lit start stop l =
    make_const start stop (Const.Lit l)

  let make_by start stop body dr =
    make_exp start stop (Raw_tree.T.EBy { body; dr; })

  let make_subty start stop ctx exp res =
    make_exp start stop (Raw_tree.T.ESub { ctx; exp; res; })

  let make_annot start stop e kind ty =
    make_exp start stop (Raw_tree.T.EAnnot { exp = e; kind; annot = ty; })

  let make_op start stop op =
    make_exp start stop (Raw_tree.T.EConst (Const.Op op))

  let make_binop_app e1 e2 e3 =
    Raw_tree.(make_app (make_app e1 e2) e3)

  let rec make_app_l e e_l =
    match e_l with
    | [] ->
       e
    | e' :: e_l ->
       make_app_l (Raw_tree.make_app e e') e_l
%}

%start<Raw_tree.T.file> file
%type<Raw_tree.T.pat> pat
%type<Raw_tree.T.exp> exp

(* Tokens; should be the same as in lexer.mli *)

%token<string> IDENT
%token<string> MODN
%token LAM
%token LPAREN
%token RPAREN
%token COMMA
%token WHERE
%token REC
%token LBRACE
%token RBRACE
%token DOT
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
%token VAL
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

%left LAM
%left WHERE
%left COLON
%left WHEN

%left SEMICOLON

%right ARR
%left PLUS MINUS
%left TIMES DIV
%nonassoc MOD
%right CONS

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
| UNIT { Type.Unit }
| BOOL { Type.Bool }
| CHAR { Type.Char }
| INT { Type.Int }
| FLOAT { Type.Float }

ty:
| bty = bty { Type.Base bty }
| STREAM bty = bty { Type.Stream bty }
| ty1 = ty TIMES ty2 = ty { Type.Prod (ty1, ty2) }
| ty1 = ty ARR ty2 = ty { Type.Fun (ty1, ty2) }
| p = warp_ty MOD ty = ty { Type.Warped (p, ty) }
| ty = paren(ty) { ty }

annot_kind:
| COLON { Source_tree.Typing }
| SUBTY { Source_tree.Subtyping }

(* Identifiers *)

longname:
| modname = MODN DOT name = IDENT { Name.make ~modname ~name }

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

(* Coercions *)

invertible:
| WRAP { Coercion.Wrap }
| UNWRAP { Coercion.Unwrap }
| CONCAT p = warp_ty q = warp_ty { Coercion.Concat (p, q) }
| DECAT p = warp_ty q = warp_ty { Coercion.Decat (p, q) }
| DIST { Coercion.Dist }
| FACT { Coercion.Fact }
| INFL { Coercion.Infl }
| DEFL { Coercion.Defl }

coercion:
| ID { Coercion.Id }
| c1 = coercion SEMICOLON c2 = coercion { Coercion.Seq (c1, c2) }
| c1 = coercion ARR c2 = coercion { Coercion.Arr (c1, c2) }
| c1 = coercion TIMES c2 = coercion { Coercion.Prod (c1, c2) }
| p = warp_ty MOD c = coercion { Coercion.Warped (p, c) }
| i = invertible { Coercion.Invertible i }
| DELAY p = warp_ty q = warp_ty { Coercion.Delay (p, q) }
| c = paren(coercion) { c }

ident_coercion:
| id = IDENT LLANGLE c = coercion { (id, c) }

coercion_ctx:
| l = separated_list(COMMA, paren(ident_coercion)) { l }

(* Patterns *)

pat_desc:
| id = IDENT
  { PVar id }
| LPAREN p1 = pat COMMA p2 = pat RPAREN
  { PPair (p1, p2) }
| LPAREN p1 = pat CONS p2 = pat RPAREN
  { PCons (p1, p2) }
| LPAREN p = pat COLON ty = ty RPAREN
  { PAnnot (p, ty) }

pat:
| pd = pat_desc { make_pat $startpos $endpos pd }

(* Definitions *)

res_ty:
| { None }
| COLON ty = ty { Some ty }

eq:
| p = pat params = list(pat) res_ty = res_ty EQUAL e = exp
    { make_eq $startpos $endpos p params res_ty e }

block:
| is_rec = boption(REC) LBRACE body = separated_list(SEMICOLON, eq) RBRACE
    { make_block $startpos $endpos is_rec body }
(* Expressions *)

simple_exp:
| id = IDENT
    { make_var $startpos $endpos id }
| ln = longname
    { make_external $startpos $endpos ln }
| l = lit
    { make_const_lit $startpos $endpos l }
| e = paren(exp)
    { e }

exp:
| e = simple_exp
    { e }
| LAM p = pat WARR e = exp %prec LAM
    { make_lam $startpos $endpos p e }
| e = simple_exp e_l = nonempty_list(simple_exp)
    { make_app_l e e_l }

| e1 = exp op = eop e2 = exp
    { make_binop_app op e1 e2 }
| e1 = exp e2 = const_exp(cwhen)
    { Raw_tree.make_app e2 e1 }
| e1 = const_exp(cmerge) e2 = simple_exp e3 = simple_exp
    { make_binop_app e1 e2 e3 }

| LPAREN e1 = exp COMMA e2 = exp RPAREN
    { make_pair $startpos $endpos e1 e2 }
| e1 = exp CONS e2 = exp
    { make_cons $startpos $endpos e1 e2 }

| e = exp WHERE block = block
    { make_where $startpos $endpos e block }
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
| LET block = block
    { make_def $startpos $endpos block }
| VAL id = IDENT COLON ty = ty
    { make_decl $startpos $endpos id ty }

file:
| body = list(phrase) EOF { make_file $startpos body }
| error { Parser_error.parsing_error $startpos $endpos }
