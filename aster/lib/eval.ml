open Ast
open Error

let empty_env = []

let extend name value env =
  (name, value) :: env

let extend_many bindings env =
  bindings @ env

let rec lookup name env =
  match env with
  | [] -> raise_error (Unbound_variable name)
  | (key, value) :: rest ->
      if String.equal key name then value else lookup name rest

let expect_bool = function
  | VBool b -> b
  | value ->
      raise_error
        (Type_error { expected = "bool"; actual = type_of_value value })

let op_to_string = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Eq -> "="
  | Lt -> "<"
  | Le -> "<="
  | Gt -> ">"
  | Ge -> ">="

let binary_type_error op left right =
  raise_error
    (Binary_type_error {
       op = op_to_string op;
       left = type_of_value left;
       right = type_of_value right;
     })

let eval_int_arith op a b =
  match op with
  | Add -> VInt (a + b)
  | Sub -> VInt (a - b)
  | Mul -> VInt (a * b)
  | Div ->
      if b = 0 then raise_error Division_by_zero else VInt (a / b)
  | Eq | Lt | Le | Gt | Ge ->
      invalid_arg "eval_int_arith: non-arithmetic operator"

let eval_float_arith op a b =
  match op with
  | Add -> VFloat (a +. b)
  | Sub -> VFloat (a -. b)
  | Mul -> VFloat (a *. b)
  | Div ->
      if Float.equal b 0.0 then raise_error Division_by_zero else VFloat (a /. b)
  | Eq | Lt | Le | Gt | Ge ->
      invalid_arg "eval_float_arith: non-arithmetic operator"

let rec equal_values left right =
  match left, right with
  | VUnit, VUnit -> true
  | VInt a, VInt b -> a = b
  | VFloat a, VFloat b -> Float.equal a b
  | VBool a, VBool b -> Bool.equal a b
  | VString a, VString b -> String.equal a b
  | VTuple xs, VTuple ys ->
      List.length xs = List.length ys && List.for_all2 equal_values xs ys
  | VClosure _, VClosure _ ->
      raise_error
        (Type_error { expected = "comparable value"; actual = TFunction })
  | _ -> false

let eval_eq left right =
  match left, right with
  | VClosure _, _ | _, VClosure _ ->
      raise_error
        (Type_error { expected = "comparable value"; actual = TFunction })
  | _ -> VBool (equal_values left right)

let eval_binop op left right =
  match op, left, right with
  | (Add | Sub | Mul | Div), VInt a, VInt b ->
      eval_int_arith op a b

  | (Add | Sub | Mul | Div), VFloat a, VFloat b ->
      eval_float_arith op a b

  | Eq, _, _ ->
      eval_eq left right

  | Lt, VInt a, VInt b -> VBool (a < b)
  | Le, VInt a, VInt b -> VBool (a <= b)
  | Gt, VInt a, VInt b -> VBool (a > b)
  | Ge, VInt a, VInt b -> VBool (a >= b)

  | Lt, VFloat a, VFloat b -> VBool (a < b)
  | Le, VFloat a, VFloat b -> VBool (a <= b)
  | Gt, VFloat a, VFloat b -> VBool (a > b)
  | Ge, VFloat a, VFloat b -> VBool (a >= b)

  | _ ->
      binary_type_error op left right

let merge_bindings left right =
  let names = List.map fst left in
  match List.find_opt (fun (name, _) -> List.mem name names) right with
  | Some (name, _) -> raise_error (Duplicate_binding name)
  | None -> left @ right

let rec match_pattern pattern value =
  match pattern, value with
  | PWild, _ ->
      Some []

  | PUnit, VUnit ->
      Some []

  | PVar name, value ->
      Some [(name, value)]

  | PInt expected, VInt actual when expected = actual ->
      Some []

  | PFloat expected, VFloat actual when Float.equal expected actual ->
      Some []

  | PBool expected, VBool actual when Bool.equal expected actual ->
      Some []

  | PString expected, VString actual when String.equal expected actual ->
      Some []

  | PTuple patterns, VTuple values ->
      match_pattern_list patterns values

  | _ ->
      None

and match_pattern_list patterns values =
  match patterns, values with
  | [], [] ->
      Some []

  | pattern :: rest_patterns, value :: rest_values ->
      begin match match_pattern pattern value with
      | None -> None
      | Some bindings ->
          begin match match_pattern_list rest_patterns rest_values with
          | None -> None
          | Some rest_bindings -> Some (merge_bindings bindings rest_bindings)
          end
      end

  | _ ->
      None

let rec eval env expr =
  match expr.node with
  | Unit ->
      VUnit

  | Int i ->
      VInt i

  | Float f ->
      VFloat f

  | Bool b ->
      VBool b

  | String s ->
      VString s

  | Var name ->
      lookup name env

  | Let (pattern, bound_expr, body) ->
      let bound_value = eval env bound_expr in
      begin match match_pattern pattern bound_value with
      | None -> raise_error (Match_failure (type_of_value bound_value))
      | Some bindings -> eval (extend_many bindings env) body
      end

  | LetRec (name, param, fn_body, body) ->
      let rec rec_env =
        (name, VClosure (param, fn_body, rec_env)) :: env
      in
      eval rec_env body

  | If (cond, then_branch, else_branch) ->
      if expect_bool (eval env cond) then
        eval env then_branch
      else
        eval env else_branch

  | Fun (param, body) ->
      VClosure (param, body, env)

  | App (fn_expr, arg_expr) ->
      let fn_value = eval env fn_expr in
      let arg_value = eval env arg_expr in
      begin match fn_value with
      | VClosure (param, body, captured_env) ->
          eval (extend param arg_value captured_env) body
      | value ->
          raise_error (Not_a_function (type_of_value value))
      end

  | Binop (left, op, right) ->
      let left_value = eval env left in
      let right_value = eval env right in
      eval_binop op left_value right_value

  | Tuple exprs ->
      VTuple (List.map (eval env) exprs)

  | Match (scrutinee, branches) ->
      let value = eval env scrutinee in
      eval_match env value branches

and eval_match env value branches =
  match branches with
  | [] ->
      raise_error (Match_failure (type_of_value value))

  | (pattern, body) :: rest ->
      begin match match_pattern pattern value with
      | None -> eval_match env value rest
      | Some bindings -> eval (extend_many bindings env) body
      end
