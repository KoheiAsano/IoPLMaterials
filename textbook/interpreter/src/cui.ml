open Eval

let rec read_eval_print env =
  print_string "# ";
  flush stdout;
  try 
    let decl = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
    (* let (id, newenv, v) = eval_decl env decl in
    Printf.printf "val %s = " id;
    pp_val v;
    print_newline(); *)
    (* let f elem =
      Printf.printf "I'm looking at element %d now\n" elem in
      (* List.iter f my_list;; *)
    let f (id, newenv, v) = 
    Printf.printf "val %s = " id;
    pp_val v;
    print_newline();
    List.iter f (eval_decl env decl); *)
    (* fはプリントの関数, declsは宣言の連結リスト,  *)
    let f = fun (id, _newenv, v) -> 
      Printf.printf "val %s = " id;
      pp_val v;
      print_newline(); in 
      let decls = eval_decl env decl in
        List.iter f (eval_decl env decl);
        match (List.hd (List.rev decls)) with 
          (_i,newenv,_v) -> read_eval_print newenv;
    
    
  with 
  | Error (s) -> Printf.printf "%s\n" s;read_eval_print env
  | Failure (s) -> Printf.printf "%s\n" s;read_eval_print env
  | _ -> Printf.printf "Fetal error \n";read_eval_print env


let initial_env =
  Environment.extend "i" (IntV 1)
  (Environment.extend "ii" (IntV 2)
  (Environment.extend "iii" (IntV 3)
  (Environment.extend "iv" (IntV 4)
    (Environment.extend "v" (IntV 5)
      (Environment.extend "x" (IntV 10) Environment.empty)))))
