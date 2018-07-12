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
                       if boo = false then 
                            let tmp_subst = [(x, TyFun(left, right))] in
                            let rec subst_type_tmp ty = subst_type tmp_subst ty in
                            let new_list = newmap subst_type_tmp rest in
                            (unify new_list)@[(x, TyFun(left, right))]
                       else err("type error!!")

            | TyInt -> let tmp_subst = [(x, TyInt)] in
                       let rec subst_type_tmp ty = subst_type tmp_subst ty in
                       let new_list = newmap subst_type_tmp rest in
                       (unify new_list)@[(x, TyInt)]

            | TyBool -> let tmp_subst = [(x, TyBool)] in
                        let rec subst_type_tmp ty = subst_type tmp_subst ty in
                        let new_list = newmap subst_type_tmp rest in
                        (unify new_list)@[(x, TyBool)]

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
                         (unify new_list)@[(x, TyInt)])
        | TyBool ->
                (match ty2 with TyInt -> err("type error!")
                 | TyBool -> unify rest
                 | TyFun(left, right) -> err("type error!")
                 | TyVar x ->   
                         let tmp_subst = [(x, TyBool)] in
                         let rec subst_type_tmp ty = subst_type tmp_subst ty in
                         let new_list = newmap subst_type_tmp rest in
                         (unify new_list)@[(x, TyBool)] )

         | TyFun(left, right) ->
                (match ty2 with TyInt -> err("type error!")
                 | TyBool -> err("type error!")
                 | TyFun(left2, right2) -> unify ((left, left2)::(right, right2)::rest)
                 | TyVar x ->   
                       let ftv =  freevar_ty (TyFun(left, right)) in
                       let boo = MySet.member (TyVar x) ftv in
                       if boo = false then 
                            let tmp_subst = [(x, TyFun(left, right))] in
                            let rec subst_type_tmp ty = subst_type tmp_subst ty in
                            let new_list = newmap subst_type_tmp rest in
                            (unify new_list)@[(x, TyFun(left, right))]
                       else err("type error!!"))
        )
     | _ -> [] )

(*eqs_of_subst : subst -> (ty * ty) list *)
let rec  eqs_of_subst s =
    match s with (left, right)::rest -> (TyVar left, right)::eqs_of_subst rest

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
          (match ty1 with TyBool ->
                (match ty2 with TyInt -> (* 途中 *)
                    (match ty3 with TyInt -> let eqs = (eqs_of_subst s1)@(eqs_of_subst s2)@(eqs_of_subst s3)@[(ty1, TyBool); (ty2, TyInt); (ty3, TyInt)]in
                                             let s4 = unify eqs in (s4, TyInt)
                        | _ -> err("type error!"))
                 | TyBool ->
                         (match ty3 with TyBool -> let eqs = (eqs_of_subst s1)@(eqs_of_subst s2)@(eqs_of_subst s3)@[(ty1, TyBool);(ty2, TyBool);(ty3, TyBool)] in
                                                  let s4 = unify eqs in (s4, TyBool)
                                | _ -> err("type error!"))
                 | TyFun(ty11, ty12) ->
                         (match ty3 with TyFun(ty21, ty22) -> let eqs = (eqs_of_subst s1)@(eqs_of_subst s2)@(eqs_of_subst s3)@[TyFun(ty11, ty12), TyFun(ty21, ty22)] in
                                                  let s4 = unify eqs in (s4, (subst_type s4 (TyFun(ty11, ty12)))  )
                                | _ -> err("type error!"))
                 | _ -> err("type error!")) 

            | _ -> err("type error!"))
          (*
          let tyarg1 = ty_exp tyenv exp1 in
          let tyarg2 = ty_exp tyenv exp2 in
          let tyarg3 = ty_exp tyenv exp3 in
          (match tyarg1 with TyBool ->
              if tyarg2 = tyarg3 then tyarg2
              else err ("Type of two arguments are not same")
          | _ -> err ("Argument must be of boolen"))
          *)

  | LetExp (id, exp1, exp2) ->
          (*
          let tyarg1 = ty_exp tyenv exp1 in
          let newtyenv = Environment.extend id tyarg1 tyenv in
          let tyarg2 = ty_exp newtyenv exp2 in
          tyarg2
          *)
          let (s1, ty1) = ty_exp tyenv exp1 in
          let newenv = Environment.extend id ty1 tyenv in
          let (s2, ty2) = ty_exp newenv exp2 in
          let eqs = (eqs_of_subst s1)@(eqs_of_subst s2) in
          let s3 = unify eqs in (s3, subst_type s3 ty2)

  | FunExp (id, exp) ->
          let domty = TyVar (fresh_tyvar) in
          let (s, ranty) =
              ty_exp (Environment.extend id domty tyenv) exp in
              (s, TyFun ((subst_type s domty), ranty))
  | AppExp (exp1, exp2) -> 
          let (s1, ty1) = ty_exp tyenv exp1 in
          let (s2, ty2) = ty_exp tyenv exp2 in
          (match ty1 with TyFun(t11, t12) ->
                   if t11 = ty2 then
                       let eqs = (eqs_of_subst s1)@(eqs_of_subst s2) in
                       let s3 = unify eqs in (s3, subst_type s3 (TyFun(t11, t12))) 
                   else err("type error!")
              | _ -> err("type error!"))
  | _ -> err ("Not Implemented!") 

(*
let ty_let tyenv = function
    Let(id, e) ->  
*)

let ty_decl tyenv = function
    Exp e -> ty_exp tyenv e
  (* | OnlyLetExp e -> ty_let tyenv e *)
  | _ -> err ("Not Implemented!")
