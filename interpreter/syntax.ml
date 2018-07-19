(* ML interpreter / type reconstruction *)
type id = string

type binOp = Plus | Mult | Lt | And | Or

type tyvar = int

exception Not_found

let rec return l x = match l with (y,z)::rest ->
                        if x = y then z else return rest x
                    | [] ->  
                            raise Not_found

(* type inference *)
type ty = TyInt | TyBool | TyVar of tyvar | TyFun of ty * ty

let stringval_of_int v =
    Char.escaped (char_of_int(v+97))

let rec sets_val_of_int v =
    let w = (v / 26) in
    if v < 26 then let x = (v mod 26) in stringval_of_int x
    else let x = (v mod 26) in (sets_val_of_int w) ^ (stringval_of_int x)

let rec pp_ty_tmp x num l = 
    (match x with
    TyInt -> print_string "int"; (num, l)
  | TyBool -> print_string "bool"; (num, l)
  | TyVar tyvar ->  
            print_string "'";
        (try let v = return l tyvar in print_string (sets_val_of_int v); (num, l)
        with _ ->  print_string (sets_val_of_int (num));
                 (num+1, [(tyvar, num)]@l) )
           
  | TyFun(ty1, ty2) -> (match ty1 with TyFun(t11,t12) ->
                            print_string "(";
                            let (num2, l2) = pp_ty_tmp ty1 num l in
                            print_string ")";
                            print_string " -> "; 
                            pp_ty_tmp ty2 num2 l2 
                        | _ -> 
                            let (num2, l2) = pp_ty_tmp ty1 num l in
                            print_string " -> ";
                            pp_ty_tmp ty2 num2 l2 ))


let pp_ty x = pp_ty_tmp x 0 []

(* Ex4.3.1 *)

 let fresh_tyvar =
 let counter = ref 0 in
 let body () =
 let v = !counter in
 counter := v + 1; v
 in body

let rec freevar_ty ty = (* ty -> tyvar Myset.t *)
    ( match ty with TyVar tyvar -> 
                MySet.singleton (TyVar tyvar)
      | TyFun (ty1, ty2) ->  MySet.union (freevar_ty ty1) (freevar_ty ty2) (* maybe wrong *)
                
      | _ -> MySet.empty )

type exp =
  | Var of id (* Var "x" --> x *)
  | ILit of int (* ILit 3 --> 3 *)
  | BLit of bool (* BLit true --> true *)
  | BinOp of binOp * exp * exp
  (* BinOp(Plus, ILit 4, Var "x") --> 4 + x *)
  | IfExp of exp * exp * exp
  (* IfExp(BinOp(Lt, Var "x", ILit 4), 
           ILit 3, 
           Var "x") --> 
     if x<4 then 3 else x *)
  | LetExp of id * exp * exp (* LetExp("x",ILit4, BinOp(Plus, ILit3, Var "x")) --> x + 3   *)
  | FunExp of id * exp
  | AppExp of exp * exp
  | LetRecExp of id * id * exp * exp

type onlyletexp  =
    | Let of id * exp
    | RecLet of id * exp * onlyletexp 
type program = 
    Exp of exp
  | OnlyLetExp of onlyletexp
  | RecDecl of id * id * exp
