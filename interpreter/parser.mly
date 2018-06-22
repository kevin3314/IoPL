%{
open Syntax
%}

%token LPAREN RPAREN SEMISEMI
%token PLUS MULT LT
%token IF THEN ELSE TRUE FALSE
%token LET IN EQ
%token FUN RARROW
%token REC
%token AND OR

%token <int> INTV
%token <Syntax.id> ID

%start toplevel
%type <Syntax.program> toplevel
%%

toplevel :
    e=Expr SEMISEMI { Exp e }
  | LET x = ID EQ e=Expr SEMISEMI { Decl (x,e) }
  | LET REC x1 = ID EQ FUN x2 = ID RARROW e=Expr SEMISEMI { RecDecl(x1,x2,e) }

Expr :
    e=IfExpr { e }
  | e=LetExpr { e }
  | e=LTExpr { e }
  | e=FunExpr { e }
  | e=LetRecExpr { e }
  | e=AndExpr { e }
  | e=OrExpr { e }
  

LTExpr : 
    l=PExpr LT r=PExpr { BinOp (Lt, l, r) }
  | e=PExpr { e }

PExpr :
    l=PExpr PLUS r=MExpr { BinOp (Plus, l, r) }
  | e=MExpr { e }

MExpr : 
    e1=MExpr MULT e2=AppExpr { BinOp (Mult, e1, e2) }
  | e=AppExpr { e }

AppExpr :
    e1=AppExpr e2=AExpr { AppExp (e1, e2) }
  | e=AExpr { e }

AExpr :
    i=INTV { ILit i }
  | TRUE   { BLit true }
  | FALSE  { BLit false }
  | i=ID   { Var i }
  | LPAREN e=Expr RPAREN { e }

IfExpr :
    IF c=Expr THEN t=Expr ELSE e=Expr { IfExp (c, t, e) }

AndExpr :
    e1=Expr AND AND e2=Expr { BinOp (And, e1, e2) }

OrExpr :
    e1=Expr OR OR e2=Expr { BinOp (Or, e1, e2) }

LetExpr :
    LET x=ID EQ e1=Expr IN e2=Expr { LetExp(x, e1, e2) }

FunExpr :
    FUN x=ID RARROW e1=Expr { FunExp(x, e1) }
    
LetRecExpr :
    LET REC x1=ID EQ FUN x2=ID RARROW e1=Expr IN e2=Expr { LetRecExp(x1, x2, e1, e2) }
