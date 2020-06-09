open Syntax

exception Error of string

let err s = raise (Error s)

(* Type Environment *)
type tyenv = ty Environment.t

let ty_prim op ty1 ty2 = match op with
  Plus -> (match ty1, ty2 with 
        TyInt, TyInt -> TyInt
      | _ -> err ("Argument must be of integer: +"))
  | Mult -> (match ty1, ty2 with 
    TyInt, TyInt -> TyInt
    | _ -> err ("Argument must be of integer: *"))
  | Lt -> (match ty1, ty2 with 
      TyInt, TyInt -> TyBool
      | _ -> err ("Argument must be of integer: <"))
  | And -> (match ty1, ty2 with 
      TyBool, TyBool -> TyBool
      | _ -> err ("Argument must be of bool: &&"))
  | Or -> (match ty1, ty2 with 
      TyBool, TyBool -> TyBool
      | _ -> err ("Argument must be of bool: ||"))
  | _ -> err "Not Implemented!"

let rec ty_exp tyenv = function
    Var x -> 
      (try Environment.lookup x tyenv with
          Environment.Not_bound -> err ("variable not bound: " ^ x))
  | ILit _ -> TyInt
  | BLit _ -> TyBool
  | BinOp (op, exp1, exp2) ->
      let tyarg1 = ty_exp tyenv exp1 in
      let tyarg2 = ty_exp tyenv exp2 in
        ty_prim op tyarg1 tyarg2
  | IfExp (exp1, exp2, exp3) ->
      let tycond = ty_exp tyenv exp1 in 
      let tythen = ty_exp tyenv exp2 in 
      let tyelse = ty_exp tyenv exp3 in 
        (match tycond with 
          TyBool -> if tythen = tyelse then tythen else err "type error"
          | _ -> err "condition must be bool")
  | LetExp (id, exp1, exp2) ->
      let tyx = ty_exp tyenv exp1 in 
      ty_exp (Environment.extend id tyx tyenv) exp2 
  | _ -> print_string " hoge"; err ("Not Implemented!")

let ty_decl tyenv = function
    Exp e -> (ty_exp tyenv e, tyenv)
  | Decl (id, e, n) -> 
    (match n with 
    Some _n -> err ("Not Implemented! consective decl")
  | None -> let tyx = ty_exp tyenv e in (tyx, Environment.extend id tyx tyenv)
  )

  | _ -> err ("Not Implemented!")

type subst = (tyvar * ty) list

let rec subst_type subs t = 
  (match t with 
    TyInt -> t
    | TyBool -> t
    | TyVar (tv) -> 
        (try 
          let _tyva,res = List.find (fun (tyv, _typ) -> tv = tyv) subs in subst_type subs res 
        with 
          Not_found -> err "given tyvar is not found"
          | _ -> err "unknown error")
    | TyFun (tyarg, tyret) -> TyFun(subst_type subs tyarg, subst_type subs tyret)
    | _ -> err "Not Implemented!")

let rec unify x = 
  try 
    let c = List.hd x in 
      let x' = List.tl x in
      (match c with 
        TyVar (tv1), TyVar (tv2) when tv1 = tv2 -> unify x'
        | TyFun (tyarg1, tyret1), TyFun(tyarg2, tyret2) -> unify (List.cons (tyret1, tyret2) (List.cons (tyarg1, tyarg2) x'))
        | TyVar (tv1), ty1 -> if MySet.member tv1 (freevar_ty ty1) then err "type is ill-defined" else [(tv1,ty1)]::unify x'
        | ty1, TyVar (tv1) -> if MySet.member tv1 (freevar_ty ty1) then err "type is ill-defined" else [(tv1,ty1)]::unify x'
        | _, _ -> err "type error"
      )
  with 
    Failure (_s) -> []
    | _  -> err ""