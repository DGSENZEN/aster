open Format
open Ast

val pp_expr : formatter -> expr -> unit
val pp_value : formatter -> value -> unit
val pp_branch: formatter -> pattern * expr -> unit
val pp_pattern: formatter -> pattern -> unit
val expr_to_string: expr -> string
val value_to_string: value -> string