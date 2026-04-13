open Ast

type env = (string * int) list

val lookup : string -> env -> int
val eval: env -> Ast.expr -> int 