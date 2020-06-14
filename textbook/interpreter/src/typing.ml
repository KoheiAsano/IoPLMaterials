open Syntax

exception Error of string

let err s = raise (Error s)

(* Type Environment *)
type tyenv = ty Environment.t


type subst = (tyvar * ty) list

let rec subst_type (subs: (tyvar * ty) list) (t: ty) :ty  = 
  (match t with 
    TyInt -> t
    | TyBool -> t
    | TyVar (tv) -> 
        (try 
          let _tyva,res = List.find (fun (tyv, _typ) -> tv = tyv) subs in subst_type subs res 
        with 
          Not_found -> TyVar(tv)
          | _ -> err "unknown error")
    | TyFun (tyarg, tyret) -> TyFun(subst_type subs tyarg, subst_type subs tyret)
    | _ -> err "Not Implemented!")
    

let pp_subst (subs: (tyvar * ty) list) = 
  print_string "subs";
  List.iter (fun (tv, ty) -> print_int tv;print_string "->"; pp_ty ty) subs;
  print_newline ()

let pp_eqs (eqs: (ty * ty) list) = 
  print_string "eqs";
  List.iter (fun (lhs, rhs) -> pp_ty lhs;print_string "="; pp_ty rhs) eqs;
  print_newline ()


(* 型代入を等式にする *)
let eqs_of_subst (s: subst) : ( ty * ty ) list = 
  List.map (fun (tyv, typ) -> (TyVar(tyv), typ)) s

(* 等式集合に型代入を適用 *)
let subst_eqs (s: subst) (eqs: ( ty * ty ) list) = 
  let subs tyv = (
      try 
        let (_, subs_val) = List.find (fun (lhs, _) -> lhs = tyv) s in
        subs_val
      with
        _ -> TyVar(tyv))
  in 
  List.map (fun (lhs, rhs) -> 
    match lhs, rhs with 
        TyVar(tv1), TyVar(tv2) -> (subs tv1, subs tv2)
      | TyVar(tv1), ty -> (subs tv1, ty)
      | ty, TyVar(tv1) -> ( ty,subs tv1)
      | ty1, ty2 -> (ty1, ty2)
  ) eqs
  
let rec unify x : (tyvar * ty) list= 
  try 
    let c = List.hd x in 
      let x' = List.tl x in
      (match c with 
        ty1, ty2 when ty1 = ty2
          -> unify x'
        | TyFun (tyarg1, tyret1), TyFun(tyarg2, tyret2) 
          -> unify (List.cons (tyret1, tyret2) (List.cons (tyarg1, tyarg2) x'))
        | TyVar (tv1), ty1 when not (MySet.member tv1 (freevar_ty ty1)) -> List.cons (tv1,ty1) (unify x')
        | ty1, TyVar (tv1) when not (MySet.member tv1 (freevar_ty ty1)) -> List.cons (tv1,ty1) (unify x')
        | _, _ -> err "type is ill-defined in unify"
      )
  with 
    Failure (_s) -> []
    (* | _  -> err "something wrong" *)

(* operationが作る等式 + 返り値の型*)
let ty_prim op ty1 ty2 = match op with
    Plus -> ([(ty1, TyInt); (ty2, TyInt)], TyInt)
  | Mult -> ([(ty1, TyInt); (ty2, TyInt)], TyInt)
  | Lt -> ([(ty1, TyInt); (ty2, TyInt)], TyBool)
  | And -> ([(ty1, TyBool); (ty2, TyBool)], TyBool)
  | Or -> ([(ty1, TyBool); (ty2, TyBool)], TyBool)

(* 型代入とexpのペアを返す *)
let rec ty_exp tyenv exp : (tyvar * ty) list * ty = 
  match exp with
    Var x -> 
    (try ([], Environment.lookup x tyenv) with 
      Environment.Not_bound -> err ("variable not bound: " ^ x))
  | ILit _ -> ([], TyInt)
  | BLit _ -> ([], TyBool)
  | BinOp (op, exp1, exp2) -> 
    let (s1, ty1) = ty_exp tyenv exp1 in 
    (* pp_subst s1; *)
    let (s2, ty2) = ty_exp tyenv exp2 in 
    (* pp_subst s2; *)
    let (eqs3, ty) = ty_prim op ty1 ty2 in 
    (* pp_eqs eqs3; *)
    let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @ eqs3 in 
    (* pp_eqs eqs; *)
    let s3 = unify eqs in (s3, subst_type s3 ty)
  | IfExp (exp1, exp2, exp3) -> 
    let (s1, ty1) = ty_exp tyenv exp1 in 
    let (s2, ty2) = ty_exp tyenv exp2 in 
    let (s3, ty3) = ty_exp tyenv exp3 in 
    let eqs = [(ty1, TyBool);(ty2, ty3);] @(eqs_of_subst s1) @ (eqs_of_subst s2) @ (eqs_of_subst s3) in
    let resub = unify eqs in (resub, subst_type resub ty3)
  | LetExp (id, exp1, exp2) -> 
    let (sx, tyx) = ty_exp tyenv exp1 in 
    let (s1, ty) = ty_exp (Environment.extend id tyx tyenv) exp2 in 
    let eqs = (eqs_of_subst sx) @ (eqs_of_subst s1) in 
    let s2 = unify eqs in (s2, subst_type s2 ty)
  | FunExp (id, exp) ->
    (* domain type *)
    let domty = TyVar (fresh_tyvar ()) in
    let s, ranty = 
      ty_exp (Environment.extend id domty tyenv) exp in 
      (s, TyFun (subst_type s domty, ranty))
  | AppExp (exp1, exp2) -> 
    let (s1, ty1) = ty_exp tyenv exp1 in 
    let (s2, ty2) = ty_exp tyenv exp2 in 
    (match ty1 with 
      TyFun(tyarg, tyret) -> 
      let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @[(tyarg,ty2);] in 
      let s3 = unify eqs in (s3, subst_type s3 tyret)
    | _ -> err "not function type is applied")
  | LetRecExp (id1, id2, exp1, exp2) -> 
    let domty = TyVar (fresh_tyvar ()) in 
    let retty = TyVar (fresh_tyvar ()) in
    let s1, ty1 = ty_exp (Environment.extend id1 (TyFun (domty,retty)) (Environment.extend id2 domty tyenv)) exp1 in 
    let s2, ty2 = ty_exp (Environment.extend id1 (TyFun (domty,ty1)) tyenv) exp2 in 
    let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @ [(retty, ty1);]in 
    let s3 = unify eqs in (s3, subst_type s3 ty2)

    

  let ty_decl tyenv = function
    Exp e -> let (_, ty) = ty_exp tyenv e in 
    (ty, tyenv)
  | Decl (id, e, n) -> 
    (match n with 
    Some _n -> err ("Not Implemented! consective decl")
  | None -> let (_, tyx) = ty_exp tyenv e in (tyx, Environment.extend id tyx tyenv)
  )

  | _ -> err ("Not Implemented!")