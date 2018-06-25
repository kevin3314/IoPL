(* ML interpreter / type reconstruction *)
type id = string

type binOp = Plus | Mult | Lt | And | Or

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

type letexp =
    | Decl of id * exp
    | NeoDecl of id * exp * letexp
  
type program = 
    Exp of exp
  | OnlyLetExp of letexp
  | RecDecl of id * id * exp
