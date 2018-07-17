open Syntax
open Eval

type subst = (tyvar * ty) list

let rec map f = function
    [] -> []
    | x :: rest -> f x :: map f rest

let rec newmap f = function
    [] -> []
    | (x, y)::rest -> (f x, f y):: newmap f rest

(* ある型の中の型変数を代入する関数 *)
let rec insert_type (id, value) ty =
    (match ty with TyVar x -> 
                if x = id then value else TyVar x
        | TyFun (left, right) ->
                TyFun( insert_type (id,value) left, insert_type (id,value) right)
        | TyInt -> TyInt
        | TyBool -> TyBool ) 

(* 実際に型変数に対し写像の列を作用させる関数 (subset -> ty) -> ty Ex4.3.2*)
let rec subst_type_tmp subst ty =
    match subst with [] -> ty
        | (id, value) :: rest ->
                let newty = insert_type (id, value) ty in
                subst_type_tmp rest newty

let rec subst_type subst ty = 
    let newty = subst_type_tmp subst ty in
    if (ty = newty) then newty else subst_type subst newty

let rec print_type = function
    TyInt -> print_string "Int"
    | TyBool -> print_string "Bool"
    | TyFun(left,right) -> print_type left; print_string "->"; print_type right
    | TyVar tyvar -> print_string "Tyvar"; print_string (string_of_int tyvar)

let rec print_subst = function
    [] -> print_newline ()
    | (tyvar, ty) :: rest -> print_string "TyVar";
                     print_string (string_of_int tyvar);
                     print_string " -> ";
                     print_type ty;
                     print_newline ();
                     print_subst rest

(* Ex 4.3.3 , 代入の組subset を返す
   (ty * ty) list -> subset*)

let rec unify l = (* 
    print_string "unifying...";
    print_newline(); *)
    (match l with (ty1, ty2) :: rest ->
        ( match ty1 with TyVar x ->
            (match ty2 with TyFun(left,right) -> 
                       let ftv =  freevar_ty (TyFun(left, right)) in
                       let boo = MySet.member (TyVar x) ftv in
                       if boo = false then 
                            let tmp_subst = [(x, TyFun(left, right))] in
                            let rec subst_type_tmp ty = subst_type tmp_subst ty in
                            let new_list = newmap subst_type_tmp rest in
                            (unify new_list)@[(x, TyFun(left, right))]
                       else err("unify error!! : ftv error")

            | TyInt -> let tmp_subst = [(x, TyInt)] in
                       let rec subst_type_tmp ty = subst_type tmp_subst ty in
                       let new_list = newmap subst_type_tmp rest in
                       (unify new_list)@[(x, TyInt)];

            | TyBool -> let tmp_subst = [(x, TyBool)] in
                        let rec subst_type_tmp ty = subst_type tmp_subst ty in
                        let new_list = newmap subst_type_tmp rest in
                        (unify new_list)@[(x, TyBool)]

            | TyVar y -> let tmp_subst = [(x, TyVar y)] in
                         let rec subst_type_tmp ty = subst_type tmp_subst ty in
                         let new_list = newmap subst_type_tmp rest in
                         (unify new_list)@[(x, TyVar y)]

            | _ (*error*)-> err("unify error!!") ) 
        
        | TyInt -> 
                (match ty2 with TyInt -> unify rest
                 | TyBool -> err("unify error! : Int - Bool")
                 | TyFun(left, right) -> err("unify error! : Int - Fun")
                 | TyVar x ->   
                         let tmp_subst = [(x, TyInt)] in
                         let rec subst_type_tmp ty = subst_type tmp_subst ty in
                         let new_list = newmap subst_type_tmp rest in
                         (unify new_list)@[(x, TyInt)])
        | TyBool ->
                (match ty2 with TyInt -> err("unify error! : Bool - Int")
                 | TyBool -> unify rest
                 | TyFun(left, right) -> err("unify error! : Bool - Fun")
                 | TyVar x ->   
                         let tmp_subst = [(x, TyBool)] in
                         let rec subst_type_tmp ty = subst_type tmp_subst ty in
                         let new_list = newmap subst_type_tmp rest in
                         (unify new_list)@[(x, TyBool)] )

         | TyFun(left, right) ->
                (match ty2 with TyInt -> err("unify error! : Fun - Int")
                 | TyBool -> err("unify error! : Fun - Bool")
                 | TyFun(left2, right2) -> unify ((left, left2)::(right, right2)::rest)
                 | TyVar x ->   
                       let ftv =  freevar_ty (TyFun(left, right)) in
                       let boo = MySet.member (TyVar x) ftv in
                       if boo = false then 
                            let tmp_subst = [(x, TyFun(left, right))] in
                            let rec subst_type_tmp ty = subst_type tmp_subst ty in
                            let new_list = newmap subst_type_tmp rest in
                            (unify new_list)@[(x, TyFun(left, right))]
                       else err("unify error!! : ftv error"))
        )
     | _ -> [] )

(*eqs_of_subst : subst -> (ty * ty) list *)
let rec  eqs_of_subst s =
(*    print_string "eqs_of_subst...";
    print_newline (); *)
    match s with (left, right)::rest -> (TyVar left, right)::eqs_of_subst rest
    |  [] -> []

(*subst_eqs: subst -> (ty * ty) list -> (ty * ty) list *)
let subst_eqs s eqs = let subst_type_tmp ty = subst_type s ty in
                      newmap subst_type_tmp eqs
    
(*
let rec subst_type subst ty =
    match subst with [] -> ty
        | (id, value) :: rest ->
                let newty = insert_type (id, value) ty in
                subst_type rest newty
*)

(* Type Environment *)
type tyenv = ty Environment.t

let ty_prim op ty1 ty2 = match op with
    Plus -> ([(ty1, TyInt); (ty2, TyInt)], TyInt)
  | Mult -> ([(ty1, TyInt); (ty2, TyInt)], TyInt)
  | Lt ->   ([(ty1, TyInt); (ty2, TyInt)], TyBool)
  | And ->  ([(ty1, TyBool); (ty2, TyBool)], TyBool)
  | Or ->   ([(ty1, TyBool); (ty2, TyBool)], TyBool)

let rec ty_exp tyenv (* return (subst, type) *) = function
    Var x ->
        (try ([], Environment.lookup x tyenv) with
          Environment.Not_bound -> err ("variable not bound: " ^ x))
  | ILit _ -> ([], TyInt)
  | BLit _ -> ([], TyBool)
  | BinOp (op, exp1, exp2) ->
          let (s1, ty1) = ty_exp tyenv exp1 in
          let (s2, ty2) = ty_exp tyenv exp2 in
          let (eqs3, ty) = ty_prim op ty1 ty2 in
          let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @ eqs3 in
          let s3 = unify eqs in (s3, subst_type s3 ty)

  | IfExp (exp1, exp2, exp3) ->
          let (s1, ty1) = ty_exp tyenv exp1 in
          let (s2, ty2) = ty_exp tyenv exp2 in
          let (s3, ty3) = ty_exp tyenv exp3 in
          let eqs = (eqs_of_subst s1)@(eqs_of_subst s2)@(eqs_of_subst s3)@[(ty1, TyBool); (ty2,ty3)] in
          let s4 = unify eqs in 
          (s4, subst_type s4 ty2)

  | LetExp (id, exp1, exp2) ->
          let (s1, ty1) = ty_exp tyenv exp1 in
          let newenv = Environment.extend id ty1 tyenv in
          let (s2, ty2) = ty_exp newenv exp2 in
          let eqs = (eqs_of_subst s1)@(eqs_of_subst s2) in
          let s3 = unify eqs in (s3, subst_type s3 ty2)

  | FunExp (id, exp) ->
          let domty = TyVar (fresh_tyvar ()) in
          let (s, ranty) =
              ty_exp (Environment.extend id domty tyenv) exp in 
              (s, TyFun ((subst_type s domty), ranty))

  | AppExp (exp1, exp2) -> 
          let (s1, ty1) = ty_exp tyenv exp1 in
          let (s2, ty2) = ty_exp tyenv exp2 in
          (match ty1 with TyFun(ty11, ty12) ->
              let eqs = (eqs_of_subst s1)@(eqs_of_subst s2)@[(ty11, ty2)] in
              let s3 = unify eqs in (s3, subst_type s3 ty12)
          | _ ->
              let tmp_tyvar = fresh_tyvar () in
              let eqs = (eqs_of_subst s1)@(eqs_of_subst s2)@[(ty1, TyFun(ty2, TyVar tmp_tyvar))] in
              let s3 = unify eqs in (s3, subst_type s3 (TyVar tmp_tyvar)))

  | _ -> err("type error! | Unknown")

let rec ty_let tyenv = function
    Let(id, e) -> let (s, t) = ty_exp tyenv e in [(id, Environment.extend id t tyenv, t)]
  | RecLet(id, e, letex) ->
          let (s, t) = ty_exp tyenv e in
          let part = (id, Environment.extend id t tyenv, t) in
          part :: ty_let (Environment.extend id t tyenv) letex

let ty_decl tyenv = function
    Exp e -> (*  print_string "typing...";
        print_newline (); *)
        let (s, t) = ty_exp tyenv e in
        [("-", tyenv, t)]
  (* | OnlyLetExp e -> ty_let tyenv e *)
  | OnlyLetExp e -> ty_let tyenv e
  | _ -> err ("Sorry, not done")
