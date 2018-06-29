open Syntax
open Eval
open Typing

let rec id_list l =
    match l with [] -> []
    | (id, env, v) :: rest -> id :: id_list rest

let rec find_id (id, env, v) l =
    match l with [] -> false
    | (id2, env2, v2) :: rest -> 
            if (id = id2) then true
            else find_id (id, env, v) rest

let rec remove_set (id, env, v) l =
    match l with [] -> []
    | (id2, env2, v2) :: rest ->
            if (id = id2) then remove_set (id, env, v) rest
            else ((id2, env2, v2) :: (remove_set (id, env, v) rest) )

let rec rem_dup_list before after =
    match before with [] -> after
    | (id, env, v) :: rest ->
            if (find_id (id, env, v) after) then rem_dup_list rest ((remove_set (id, env, v) after) @ [(id, env, v)])
            else rem_dup_list rest (after @ [(id, env, v)])
           
let rec rec_eval_environment env eval_list tyenv decl =
    match eval_list with [] -> env
    | (id, newenv, v) :: rest ->
        let ty = ty_decl tyenv decl in
        Printf.printf "val %s : " id;
        pp_ty ty;
        print_string " = ";
        pp_val v;
        print_newline();
        let newenv = Environment.extend id v env in
        rec_eval_environment newenv rest tyenv decl

let rec read_eval_print env tyenv =
  print_string "# ";
  flush stdout;
  let decl = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
  (*
  let (id, newenv, v) = eval_decl env decl in
  *)
  let eval_list = eval_decl env decl in
  let res_list = rem_dup_list eval_list [] in
  let newenv = rec_eval_environment env res_list tyenv decl in
  (*
    Printf.printf "val %s = " id;
    pp_val v;
    print_newline();
  *)
    read_eval_print newenv tyenv

let initial_env = 
   Environment.extend "iv" (IntV 4)
  (Environment.extend "iii" (IntV 3)
  (Environment.extend "ii" (IntV 2)
  (Environment.extend "i" (IntV 1)
    (Environment.extend "v" (IntV 5) 
       (Environment.extend "x" (IntV 10) Environment.empty)))))

let initial_tyenv = 
    Environment.extend "i" TyInt
        (Environment.extend "v" TyInt
            (Environment.extend "x" TyInt Environment.empty))

let rec rec_read_eval_print env tyenv =
    try ( read_eval_print env tyenv ) with Error s ->
        print_string s;
        print_newline ();
        rec_read_eval_print env tyenv
   | _ -> print_string "error !!";
          print_newline ();
          rec_read_eval_print env tyenv

let _ = rec_read_eval_print initial_env initial_tyenv
