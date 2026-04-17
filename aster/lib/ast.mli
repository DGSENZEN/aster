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

type pattern = 
  | PWild
  | PVar of string
  | PInt of int
  | PFloat of float
  | PBool of bool
  | PTuple of pattern list

type expr =
  | Int of int
  | Float of float
  | Var of string
  | Let of string * expr * expr
  | LetRec of string * expr * expr
  | Bool of bool
  | If of expr * expr * expr
  | Binop of expr * binop * expr
  | Fun of string * expr
  | App of expr * expr
  | Tuple of expr list
  | Match of expr * (pattern * expr) list

type value =
  | VInt of int
  | VFloat of float
  | VBool of bool
  | VClosure of string * expr * env 
  | VTuple of value list
  
and env = (string * value ref) list

