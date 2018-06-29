open Syntax

exception Error of string

type subst = (tyvar * ty) list

(* ある型の中の型変数を代入する関数 *)
let rec insert_type (id, value) ty =
    (match ty with TyVar x -> 
                if x = id then value else TyVar x
        | TyFun (left, right) ->
                TyFun( insert_type (id,value) left, insert_type (id,value) right)
        | TyInt -> TyInt
        | TyBool -> TyBool ) 

let rec subst_type subst ty =
    match subst with [] -> ty
        | (id, value) :: rest ->
                let newty = insert_type (id, value) ty in
                subst_type rest newty

                
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

let ty_decl tyenv = function
    Exp e -> ty_exp tyenv e
  (* | OnlyLetExp e -> ty_let tyenv e *)
  | _ -> err ("Not Implemented!")
