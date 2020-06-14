open MySet
(* ML interpreter / type reconstruction *)
type id = string

type binOp = Plus | Mult | Lt | And | Or

type exp =
    Var of id
  | ILit of int
  | BLit of bool
  | BinOp of binOp * exp * exp
  | IfExp of exp * exp * exp
  | LetExp of id * exp * exp
  | FunExp of id * exp
  | AppExp of exp * exp
  | LetRecExp of id * id * exp * exp

type program =
    Exp of exp
  | Decl of id * exp * program option 
  | RecDecl of id * id * exp

type tyvar = int
type ty =
    TyInt
  | TyBool
  | TyVar of tyvar
  | TyFun of ty * ty
  | TyList of ty

let fresh_tyvar = 
  let counter = ref 0 in 
  let body () = 
    let v = !counter in 
      counter := v + 1; v
  in body

let rec freevar_ty ty = 
  match ty with 
  | TyVar (tv) -> MySet.singleton tv
  | TyFun (tyarg, tyret) -> MySet.union (freevar_ty tyarg) (freevar_ty tyret)
  | _ -> MySet.empty

let rec pp_ty typ = 
  match typ with 
    TyInt -> print_string "int"
  | TyBool -> print_string "bool"
  | TyFun (tyarg, tyret)-> pp_ty tyarg; print_string " -> ";pp_ty tyret;
  | TyVar(tv) -> print_string "variable:"; print_int tv
  | _ -> print_string "not implemented"