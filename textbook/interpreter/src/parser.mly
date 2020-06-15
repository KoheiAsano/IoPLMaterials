%{
open Syntax
exception ParseError of string
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

IDS:
    x=ID
    { [x] }
  | x=ID a=IDS
    { x :: a }

toplevel :
    e=Expr SEMISEMI { Exp e }
  | LET l=LetBind { l }
  | LET REC x=ID EQ FUN y=ID RARROW e=Expr SEMISEMI {RecDecl (x, y, e)} 
  | LET REC x=IDS EQ e=Expr SEMISEMI { 
      match List.length x with
      l when l = 1 -> Decl (List.hd x,e, None)
    | l when l > 1 -> RecDecl(List.hd x, List.hd (List.tl x), List.fold_right (fun e a -> FunExp(e,a)) (List.tl (List.tl x)) e )
    | _ -> raise (ParseError "rec parse error")
  }

LetBind :
    x=ID EQ e=Expr SEMISEMI {Decl (x,e, None)}
  | x=IDS EQ e=Expr SEMISEMI {
      match List.length x with
      l when l = 1 -> Decl (List.hd x,e, None)
    | l when l > 1 -> Decl (List.hd x, List.fold_right (fun e a -> FunExp(e,a)) (List.tl x) e, None)
    | _ -> raise (ParseError "rec parse error")
    }
    // 下は連続let用だけど一旦放置
  | x=ID EQ e=Expr LET n=LetBind {Decl (x,e, Some(n))}

Expr :
    e=IfExpr { e }
  | e=LetExpr { e }
  | e=ORExpr { e }
  | e=FunExpr { e }
  | e=LetRecExpr { e }

ARGS:
  x=ID RARROW
  { [x] }
| x=ID a=ARGS
  { x :: a }

FunExpr :
    FUN args=ARGS e=Expr { List.fold_right (fun e a -> FunExp(e,a)) args e }


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
  | LET REC x=IDS EQ e1=Expr IN e2=Expr { match List.length x with
  l when l = 1 -> LetExp (List.hd x, e1, e2)
| l when l > 1 -> LetRecExp(List.hd x, List.hd (List.tl x), List.fold_right (fun e a -> FunExp(e,a)) (List.tl (List.tl x)) e1, e2 )
| _ -> raise (ParseError "rec parse error")
}