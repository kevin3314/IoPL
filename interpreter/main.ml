open Syntax
open Eval

let rec rec_eval_environment env eval_list =
    match eval_list with [] -> env
    | (id, newenv, v) :: rest ->
        Printf.printf "val %s = " id;
        pp_val v;
        print_newline();
        let newenv = Environment.extend id v env in
        rec_eval_environment newenv rest

let rec read_eval_print env =
  print_string "# ";
  flush stdout;
  let decl = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
  (*
  let (id, newenv, v) = eval_decl env decl in
  *)
  let eval_list = eval_decl env decl in
  let newenv = rec_eval_environment env eval_list in
  (*
    Printf.printf "val %s = " id;
    pp_val v;
    print_newline();
  *)
    read_eval_print newenv

let initial_env = 
   Environment.extend "iv" (IntV 4)
  (Environment.extend "iii" (IntV 3)
  (Environment.extend "ii" (IntV 2)
  (Environment.extend "i" (IntV 1)
    (Environment.extend "v" (IntV 5) 
       (Environment.extend "x" (IntV 10) Environment.empty)))))

let rec rec_read_eval_print env =
    try ( read_eval_print env ) with Error s ->
        print_string s;
        print_newline ();
        rec_read_eval_print env
   | _ -> print_string "error !!";
          print_newline ();
          rec_read_eval_print env

let _ = rec_read_eval_print initial_env                             
