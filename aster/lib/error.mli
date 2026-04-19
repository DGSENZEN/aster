open Ast

type t = 
| TInt 
| TFloat
| TBool
| TClosure
| TTuple

type error =
| VarError of string
| TypeError of t * t
| DivByZeroError
| MatchError of t 
| AppError of t 

exception Eval_Error of error

val val_to_type: value -> t
val type_to_string: t -> string
val error_to_string: error -> string
val raise_error: error -> 'a