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

let type_of_value = function
  | VUnit -> TUnit
  | VInt _ -> TInt
  | VFloat _ -> TFloat
  | VBool _ -> TBool
  | VString _ -> TString
  | VTuple _ -> TTuple
  | VClosure _ -> TFunction

let runtime_type_to_string = function
  | TUnit -> "unit"
  | TInt -> "int"
  | TFloat -> "float"
  | TBool -> "bool"
  | TString -> "string"
  | TTuple -> "tuple"
  | TFunction -> "function"

let error_to_string = function
  | Unbound_variable name ->
      Printf.sprintf "Unbound variable: %s" name

  | Type_error { expected; actual } ->
      Printf.sprintf
        "Type error: expected %s, got %s"
        expected
        (runtime_type_to_string actual)

  | Binary_type_error { op; left; right } ->
      Printf.sprintf
        "Type error: operator %s cannot be applied to %s and %s"
        op
        (runtime_type_to_string left)
        (runtime_type_to_string right)

  | Division_by_zero ->
      "Division by zero"

  | Match_failure typ ->
      Printf.sprintf
        "Match failure: no pattern matched value of type %s"
        (runtime_type_to_string typ)

  | Not_a_function typ ->
      Printf.sprintf
        "Application error: tried to call a %s"
        (runtime_type_to_string typ)

  | Duplicate_binding name ->
      Printf.sprintf
        "Duplicate variable in pattern: %s"
        name

let raise_error err = raise (Eval_error err)
