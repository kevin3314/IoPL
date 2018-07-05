open Syntax

exception Error of string

let err s = raise (Error s)

type subst = (tyvar * ty) list

let rec map f = function
    [] -> []
    | x :: rest -> f x :: map f rest

let rec newmap f = function
    [] -> []
    | (x, y)::rest -> (f x, f y)::(newmap f rest)

(* ある型の中の型変数を代入する関数 *)
let rec insert_type (id, value) ty =
    (match ty with TyVar x -> 
                if x = id then value else TyVar x
        | TyFun (left, right) ->
                TyFun( insert_type (id,value) left, insert_type (id,value) right)
        | TyInt -> TyInt
        | TyBool -> TyBool ) 

(* 実際に型変数に対し写像の列を作用させる関数 (subset -> ty) -> ty Ex4.3.2*)
let rec subst_type subst ty =
    match subst with [] -> ty
        | (id, value) :: rest ->
                let newty = insert_type (id, value) ty in
                subst_type rest newty

(* Ex 4.3.3 , 代入の組subset を返す
   (ty * ty) list -> subset*)

let rec unify l = 
    (match l with (ty1, ty2) :: rest ->
        ( match ty1 with TyVar x ->
            (match ty2 with TyFun(left,right) -> 
                       let ftv =  freevar_ty (TyFun(left, right)) in
                       let boo = MySet.member (TyVar x) ftv in
                       if boo = true then 
                            let tmp_subst = [(x, TyFun(left, right))] in
                            let rec subst_type_tmp ty = subst_type tmp_subst ty in
                            let new_list = newmap subst_type_tmp rest in
                            (unify new_list)@[(TyVar x, TyFun(left, right))]
                       else err("type error!!")

            | TyInt -> let tmp_subst = [(x, TyInt)] in
                       let rec subst_type_tmp ty = subst_type tmp_subst ty in
                       let new_list = newmap subst_type_tmp rest in
                       (unify new_list)@[(TyVar x, TyInt)]

            | TyBool -> let tmp_subst = [(x, TyBool)] in
                        let rec subst_type_tmp ty = subst_type tmp_subst ty in
                        let new_list = newmap subst_type_tmp rest in
                        (unify new_list)@[(TyVar x, TyBool)]

            | TyVar y -> unify rest 

            | _ (*error*)-> err("type error!!") ) 
        
        | TyInt -> 
                (match ty2 with TyInt -> unify rest
                 | TyBool -> err("type error!")
                 | TyFun(left, right) -> err("type error!")
                 | TyVar x ->   
                         let tmp_subst = [(x, TyInt)] in
                         let rec subst_type_tmp ty = subst_type tmp_subst ty in
                         let new_list = newmap subst_type_tmp rest in
                         (unify new_list)@[(TyVar x, TyInt)])
        | TyBool ->
                (match ty2 with TyInt -> err("type error!")
                 | TyBool -> unify rest
                 | TyFun(left, right) -> err("type error!")
                 | TyVar x ->   
                         let tmp_subst = [(x, TyBool)] in
                         let rec subst_type_tmp ty = subst_type tmp_subst ty in
                         let new_list = newmap subst_type_tmp rest in
                         (unify new_list)@[(TyVar x, TyBool)] )

         | TyFun(left, right) ->
                (match ty2 with TyInt -> err("type error!")
                 | TyBool -> err("type error!")
                 | TyFun(left2, right2) -> unify ((left, left2)::(right, right2)::rest)
                 | TyVar x ->   
                       let ftv =  freevar_ty (TyFun(left, right)) in
                       let boo = MySet.member (TyVar x) ftv in
                       if boo = true then 
                            let tmp_subst = [(x, TyFun(left, right))] in
                            let rec subst_type_tmp ty = subst_type tmp_subst ty in
                            let new_list = newmap subst_type_tmp rest in
                            (unify new_list)@[(TyVar x, TyFun(left, right))]
                       else err("type error!!"))
        )
     | _ -> [] )
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
               | _ -> err ("Argument must be of integer: >"))
  
  | And -> (match ty1, ty2 with 
                 TyBool, TyBool -> TyBool
               | _ -> err ("Argument must be of boolean: &&"))
  
  | Or -> (match ty1, ty2 with 
                 TyBool, TyBool -> TyBool
               | _ -> err ("Argument must be of boolean: ||"))

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
          let tyarg1 = ty_exp tyenv exp1 in
          let tyarg2 = ty_exp tyenv exp2 in
          let tyarg3 = ty_exp tyenv exp3 in
          (match tyarg1 with TyBool ->
              if tyarg2 = tyarg3 then tyarg2
              else err ("Type of two arguments are not same")
          | _ -> err ("Argument must be of boolen"))
  | LetExp (id, exp1, exp2) ->
          let tyarg1 = ty_exp tyenv exp1 in
          let newtyenv = Environment.extend id tyarg1 tyenv in
          let tyarg2 = ty_exp newtyenv exp2 in
          tyarg2
  | _ -> err ("Not Implemented!") 

(*
let ty_let tyenv = function
    Let(id, e) ->  
*)

let ty_decl tyenv = function
    Exp e -> ty_exp tyenv e
  (* | OnlyLetExp e -> ty_let tyenv e *)
  | _ -> err ("Not Implemented!")
