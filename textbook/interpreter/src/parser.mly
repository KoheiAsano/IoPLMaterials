%{
open Syntax
%}

%token LPAREN RPAREN SEMISEMI
%token PLUS MULT LT AMPAMP PIPPIP RARROW
%token IF FUN THEN ELSE TRUE FALSE 
%token LET REC IN EQ

%token <int> INTV
%token <Syntax.id> ID

%start toplevel
%type <Syntax.program> toplevel
%%

toplevel :
    e=Expr SEMISEMI { Exp e }
  | LET l=LetBind { l }
  | LET REC x=ID EQ FUN y=ID RARROW e=Expr SEMISEMI {RecDecl (x, y, e)} 

LetBind :
    x=ID EQ e=Expr SEMISEMI {Decl (x,e, None)}
  | x=ID EQ e=Expr LET n=LetBind {Decl (x,e, Some(n))}

Expr :
    e=IfExpr { e }
  | e=LetExpr { e }
  | e=ORExpr { e }
  | e=FunExpr { e }
  | e=LetRecExpr { e }

FunExpr :
  FUN x=ID RARROW e=Expr { FunExp (x, e) }

ORExpr :
    l=ANDExpr PIPPIP r=ORExpr { BinOp (Or, l, r) }
  | e=ANDExpr { e }

ANDExpr :
    l=LTExpr AMPAMP r=ANDExpr { BinOp (And, l, r) }
  | e=LTExpr { e }

LTExpr :
    l=PExpr LT r=PExpr { BinOp (Lt, l, r) }
  | e=PExpr { e }

PExpr :
    l=PExpr PLUS r=MExpr { BinOp (Plus, l, r) }
  | e=MExpr { e }

MExpr :
    l=MExpr MULT r=AppExpr { BinOp (Mult, l, r) }
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

LetExpr : 
    LET x=ID EQ e1=Expr IN e2=Expr { LetExp (x, e1, e2) }

LetRecExpr :
    LET REC x=ID EQ FUN y=ID RARROW e1=Expr IN e2=Expr { LetRecExp (x, y, e1, e2) }