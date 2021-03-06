%{
open Syntax
open GSR
%}

%token LPAREN RPAREN SEMI SEMISEMI COLON SLASH CARET SHARP
%token PLUS MINUS STAR QUESTION
%token FUN RARROW TRUE FALSE INT BOOL UNIT SHIFT RESET
%token IF THEN ELSE
%token EQUAL GT LT

%token <int> INTV
%token <Syntax.id> ID

%start toplevel
%type <Syntax.GSR.program> toplevel

(* Ref: https://caml.inria.fr/pub/docs/manual-ocaml/expr.html *)
%right RARROW
%right SEMI
%right prec_if
%left  EQUAL GT LT
%left  PLUS MINUS
%left  STAR SLASH
%right prec_app

%%

toplevel :
  | Expr SEMISEMI { Exp $1 }
  | SHARP Directive SEMISEMI { Directive $2 }

Expr :
  | Expr SEMI Expr { Consq ($1, $3) }
  | IF Expr THEN Expr ELSE Expr { If ($2, $4, $6) } %prec prec_if
  | FUN OptionalAnswerTypeAnnot ID RARROW Expr { Fun ($2, $3, Typing.GSR.fresh_tyvar (), $5) }
  | FUN OptionalAnswerTypeAnnot LPAREN ID COLON Type RPAREN RARROW Expr { Fun ($2, $4, $6, $9) }
  | Expr EQUAL Expr { BinOp (Equal, $1, $3) }
  | Expr GT Expr { BinOp (Gt, $1, $3) }
  | Expr LT Expr { BinOp (Lt, $1, $3) }
  | Expr STAR Expr { BinOp (Mult, $1, $3) }
  | Expr SLASH Expr { BinOp (Div, $1, $3) }
  | Expr PLUS Expr { BinOp (Plus, $1, $3) }
  | Expr MINUS Expr { BinOp (Minus, $1, $3) }
  | SimpleExpr SimpleExpr { App ($1, $2) } (* %prec prec_app *)
  | RESET OptionalAnswerTypeAnnot Expr { Reset ($3, $2) } %prec prec_app
  | SHIFT ID RARROW Expr { Shift ($2, Typing.GSR.fresh_tyvar (), $4) } %prec prec_app
  | SHIFT LPAREN ID COLON Type RPAREN RARROW Expr { Shift ($3, $5, $8) } %prec prec_app
  | SimpleExpr { $1 }

SimpleExpr :
  | INTV { Const (ConstInt $1) }
  | TRUE { Const (ConstBool true) }
  | FALSE { Const (ConstBool false) }
  | LPAREN RPAREN { Const ConstUnit }
  | ID { Var $1 }
  | LPAREN Expr RPAREN { $2 }

Type :
  | AType SLASH AType RARROW AType SLASH AType  { TyFun ($1, $3, $5, $7) }
  | AType { $1 }

AType :
  | LPAREN Type RPAREN { $2 }
  | INT { TyBase TyInt }
  | BOOL { TyBase TyBool }
  | UNIT { TyBase TyUnit }
  | QUESTION { TyDyn }

OptionalAnswerTypeAnnot :
  | { Typing.GSR.fresh_tyvar () }
  | CARET Type { $2 }

Directive :
  | ID TRUE { BoolDir ($1, true) }
  | ID FALSE { BoolDir ($1, false) }
