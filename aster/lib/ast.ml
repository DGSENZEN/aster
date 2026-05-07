open Common

type binop =
  | Add
  | Sub
  | Mul
  | Div
  | Eq
  | Lt
  | Le
  | Gt
  | Ge

type pattern =
  | PWild
  | PUnit
  | PVar of string
  | PInt of int
  | PFloat of float
  | PBool of bool
  | PString of string
  | PTuple of pattern list

type expr_node =
  | Unit
  | Int of int
  | Float of float
  | Bool of bool
  | String of string
  | Var of string
  | Let of pattern * expr * expr
  | LetRec of string * string * expr * expr
  | If of expr * expr * expr
  | Fun of string * expr
  | App of expr * expr
  | Binop of expr * binop * expr
  | Tuple of expr list
  | Match of expr * (pattern * expr) list

and expr = {
  node : expr_node;
  loc : loc;
}

type value =
  | VUnit
  | VInt of int
  | VFloat of float
  | VBool of bool
  | VString of string
  | VTuple of value list
  | VClosure of string * expr * env

and env = (string * value) list

let mk ?(loc = dummy_loc) node = { node; loc }

let unit = mk Unit
let int i = mk (Int i)
let float f = mk (Float f)
let bool b = mk (Bool b)
let string s = mk (String s)
let var name = mk (Var name)
let tuple exprs = mk (Tuple exprs)
let let_ pat bound body = mk (Let (pat, bound, body))
let let_name name bound body = let_ (PVar name) bound body
let let_rec name param fn_body body = mk (LetRec (name, param, fn_body, body))
let if_ cond then_ else_ = mk (If (cond, then_, else_))
let fun_ param body = mk (Fun (param, body))
let app fn arg = mk (App (fn, arg))
let binop left op right = mk (Binop (left, op, right))
let match_ scrutinee branches = mk (Match (scrutinee, branches))

let add a b = binop a Add b
let sub a b = binop a Sub b
let mul a b = binop a Mul b
let div a b = binop a Div b
let eq a b = binop a Eq b
let lt a b = binop a Lt b
let le a b = binop a Le b
let gt a b = binop a Gt b
let ge a b = binop a Ge b
