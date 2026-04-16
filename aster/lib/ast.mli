type binop = 
  | Add
  | Sub
  | Mul 
  | Div 
  | Eq 
  | Lt 
  | Gt 
  | Le 
  | Ge

type expr =
  | Int of int
  | Var of string
  | Let of string * expr * expr
  | Bool of bool
  | If of expr * expr * expr
  | Binop of expr * binop * expr
  | Fun of string * expr
  | App of expr * expr

type value =
  | VInt of int
  | VBool of bool
  | VClosure of string * expr * env 

and env = (string * value) list
