open Ast

val lookup : string -> env -> value
val eval: env -> expr -> value 