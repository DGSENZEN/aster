open Ast

type value = 
    | VInt of int

type env = (string * value) list

val lookup : string -> env -> value
val eval: env -> expr -> value 