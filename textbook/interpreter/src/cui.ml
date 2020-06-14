open Eval
open Typing
open Syntax

let rec read_eval_print env tyenv =
  print_string "# ";
  flush stdout;
  try 
    let decl = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
    let ty, newtyenv = ty_decl tyenv decl in 
    (* fはプリントの関数, declsは宣言の連結リスト,  *)
    let f = fun (id, _newenv, v) -> 
      Printf.printf "val %s : " id;
      pp_ty ty 0;
      print_string " = ";
      pp_val v;
      print_newline(); in 
      let decls = eval_decl env decl in
        List.iter f (eval_decl env decl);
        match (List.hd (List.rev decls)) with 
          (_i,newenv,_v) -> read_eval_print newenv newtyenv;
    
    
  with 
  | Error (s) -> Printf.printf "%s\n" s;read_eval_print env tyenv
  | Failure (s) -> Printf.printf "%s\n" s;read_eval_print env tyenv
  | _ -> Printf.printf "Fetal error \n";read_eval_print env tyenv

let initial_tyenv = 
  Environment.extend "i" TyInt
  (Environment.extend "ii" TyInt
  (Environment.extend "iii" TyInt
  (Environment.extend "iv" TyInt
    (Environment.extend "v" TyInt
      (Environment.extend "x" TyInt Environment.empty)))))
let initial_env =
  Environment.extend "i" (IntV 1)
  (Environment.extend "ii" (IntV 2)
  (Environment.extend "iii" (IntV 3)
  (Environment.extend "iv" (IntV 4)
    (Environment.extend "v" (IntV 5)
      (Environment.extend "x" (IntV 10) Environment.empty)))))
