type expr =
  | Int of int
  | Var of string
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Let of string * expr * expr
  | Bool of bool
  (*| If of expr * expr * expr *)

type value =
  | VInt of int
  | VBool of bool

type env = (string * value) list