open Ast

val empty_env : env
val extend : string -> value -> env -> env
val extend_many : (string * value) list -> env -> env
val lookup : string -> env -> value
val match_pattern : pattern -> value -> (string * value) list option
val eval : env -> expr -> value
