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


let val_to_type = function 
| VInt _ -> TInt
| VFloat _ -> TFloat
| VBool _ -> TBool
| VClosure _ -> TClosure
| VTuple _ -> TTuple 


let type_to_string = function
| TInt -> "integer"
| TFloat -> "float"
| TBool -> "boolean"
| TClosure -> "<fun>"
| TTuple -> "tuple"

let error_to_string = function
| VarError x -> Printf.sprintf "Unbound variable error: %s" x
| TypeError (a, b) -> let a_string = type_to_string a in 
  let b_string = type_to_string b in 
  Printf.sprintf "Type Error: expected %s but got a %s"  a_string b_string
| DivByZeroError -> "Division by zero!"
| MatchError a -> let match_e = type_to_string a in Printf.sprintf "Match Error: No pattern matched of type %s" match_e
| AppError a -> let app_e = type_to_string a in Printf.sprintf "Application error: tried to call a value of type %s" app_e 

exception Eval_Error of error
let raise_error err = raise (Eval_Error err)

