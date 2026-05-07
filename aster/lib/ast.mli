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

val mk : ?loc:loc -> expr_node -> expr

val unit : expr
val int : int -> expr
val float : float -> expr
val bool : bool -> expr
val string : string -> expr
val var : string -> expr
val tuple : expr list -> expr
val let_ : pattern -> expr -> expr -> expr
val let_name : string -> expr -> expr -> expr
val let_rec : string -> string -> expr -> expr -> expr
val if_ : expr -> expr -> expr -> expr
val fun_ : string -> expr -> expr
val app : expr -> expr -> expr
val binop : expr -> binop -> expr -> expr
val match_ : expr -> (pattern * expr) list -> expr

val add : expr -> expr -> expr
val sub : expr -> expr -> expr
val mul : expr -> expr -> expr
val div : expr -> expr -> expr
val eq : expr -> expr -> expr
val lt : expr -> expr -> expr
val le : expr -> expr -> expr
val gt : expr -> expr -> expr
val ge : expr -> expr -> expr
