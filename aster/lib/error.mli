open Ast

type runtime_type =
  | TUnit
  | TInt
  | TFloat
  | TBool
  | TString
  | TTuple
  | TFunction

type error =
  | Unbound_variable of string
  | Type_error of { expected : string; actual : runtime_type }
  | Binary_type_error of { op : string; left : runtime_type; right : runtime_type }
  | Division_by_zero
  | Match_failure of runtime_type
  | Not_a_function of runtime_type
  | Duplicate_binding of string

exception Eval_error of error

val type_of_value : value -> runtime_type
val runtime_type_to_string : runtime_type -> string
val error_to_string : error -> string
val raise_error : error -> 'a
