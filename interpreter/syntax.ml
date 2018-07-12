(* ML interpreter / type reconstruction *)
type id = string

type binOp = Plus | Mult | Lt | And | Or

type tyvar = int

(* type inference *)
type ty = TyInt | TyBool | TyVar of tyvar | TyFun of ty * ty

let rec pp_ty = function (* maybe wrong *)
    TyInt -> print_string "int"
  | TyBool -> print_string "bool"
  | TyVar tyvar -> print_string "'a"
  | TyFun (ty1, ty2) -> pp_ty ty1; print_string " -> "; pp_ty ty2


(* Ex4.3.1 *)
let fresh_tyvar =
    let counter = ref 0 in
    let body () =
        let v = !counter in
        counter := v + 1; v
    in body () 

let rec freevar_ty ty = (* ty -> tyvar Myset.t *)
    ( match ty with TyVar tyvar -> 
                MySet.insert ( TyVar fresh_tyvar ) MySet.empty
      | TyFun (ty1, ty2) -> 
                MySet.insert (TyVar fresh_tyvar) ( freevar_ty ty2 )
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
