open Format
open Ast

let pp_binop ppf = function
  | Add -> fprintf ppf "+"
  | Sub -> fprintf ppf "-"
  | Mul -> fprintf ppf "*"
  | Div -> fprintf ppf "/"
  | Eq -> fprintf ppf "="
  | Lt -> fprintf ppf "<"
  | Le -> fprintf ppf "<="
  | Gt -> fprintf ppf ">"
  | Ge -> fprintf ppf ">="

let pp_float ppf f =
  fprintf ppf "%g" f

let binop_prec = function
  | Eq | Lt | Le | Gt | Ge -> 1
  | Add | Sub -> 2
  | Mul | Div -> 3

let expr_prec expr =
  match expr.node with
  | Let _ | LetRec _ | If _ | Fun _ | Match _ -> 0
  | Binop (_, op, _) -> binop_prec op
  | App _ -> 4
  | Unit | Int _ | Float _ | Bool _ | String _ | Var _ | Tuple _ -> 5

let rec pp_expr ppf expr =
  pp_expr_prec 0 ppf expr

and pp_expr_prec ctx_prec ppf expr =
  let my_prec = expr_prec expr in
  if my_prec < ctx_prec then
    fprintf ppf "(@[%a@])" (pp_expr_prec 0) expr
  else
    match expr.node with
    | Unit ->
        fprintf ppf "()"

    | Int i ->
        fprintf ppf "%d" i

    | Float f ->
        pp_float ppf f

    | Bool b ->
        fprintf ppf "%b" b

    | String s ->
        fprintf ppf "%S" s

    | Var name ->
        fprintf ppf "%s" name

    | Let (pattern, bound, body) ->
        fprintf ppf
          "@[<v 2>let %a =@,%a@]@,@[<v 2>in@,%a@]"
          pp_pattern pattern
          pp_expr bound
          pp_expr body

    | LetRec (name, param, fn_body, body) ->
        fprintf ppf
          "@[<v 2>let rec %s %s =@,%a@]@,@[<v 2>in@,%a@]"
          name
          param
          pp_expr fn_body
          pp_expr body

    | If (cond, then_branch, else_branch) ->
        fprintf ppf
          "@[<v 2>if %a then@,%a@]@,@[<v 2>else@,%a@]"
          pp_expr cond
          pp_expr then_branch
          pp_expr else_branch

    | Fun (param, body) ->
        fprintf ppf
          "@[<hov 2>fn %s ->@ %a@]"
          param
          pp_expr body

    | App (fn_expr, arg_expr) ->
        fprintf ppf
          "@[<hov 2>%a@ %a@]"
          (pp_expr_prec 4) fn_expr
          (pp_expr_prec 5) arg_expr

    | Binop (left, op, right) ->
        pp_binop_expr ppf left op right

    | Tuple exprs ->
        pp_expr_tuple ppf exprs

    | Match (scrutinee, branches) ->
        fprintf ppf
          "@[<v 2>match %a with@,%a@]"
          pp_expr scrutinee
          pp_branches branches

and pp_binop_expr ppf left op right =
  let prec = binop_prec op in
  fprintf ppf
    "@[<hov 2>%a@ %a@ %a@]"
    (pp_expr_prec prec) left
    pp_binop op
    (pp_expr_prec (prec + 1)) right

and pp_expr_tuple ppf exprs =
  match exprs with
  | [] ->
      fprintf ppf "()"

  | [expr] ->
      fprintf ppf "(%a,)" pp_expr expr

  | exprs ->
      fprintf ppf
        "(@[<hov>%a@])"
        (pp_print_list
           ~pp_sep:(fun ppf () -> fprintf ppf ",@ ")
           pp_expr)
        exprs

and pp_pattern ppf = function
  | PWild ->
      fprintf ppf "_"

  | PUnit ->
      fprintf ppf "()"

  | PVar name ->
      fprintf ppf "%s" name

  | PInt i ->
      fprintf ppf "%d" i

  | PFloat f ->
      pp_float ppf f

  | PBool b ->
      fprintf ppf "%b" b

  | PString s ->
      fprintf ppf "%S" s

  | PTuple patterns ->
      pp_pattern_tuple ppf patterns

and pp_pattern_tuple ppf patterns =
  match patterns with
  | [] ->
      fprintf ppf "()"

  | [pattern] ->
      fprintf ppf "(%a,)" pp_pattern pattern

  | patterns ->
      fprintf ppf
        "(@[<hov>%a@])"
        (pp_print_list
           ~pp_sep:(fun ppf () -> fprintf ppf ",@ ")
           pp_pattern)
        patterns

and pp_branch ppf (pattern, body) =
  fprintf ppf
    "@[<hov 2>| %a ->@ %a@]"
    pp_pattern pattern
    pp_expr body

and pp_branches ppf branches =
  pp_print_list
    ~pp_sep:(fun ppf () -> fprintf ppf "@,")
    pp_branch
    ppf
    branches

let expr_to_string expr =
  asprintf "%a" pp_expr expr

let rec pp_value ppf = function
  | VUnit ->
      fprintf ppf "()"

  | VInt i ->
      fprintf ppf "%d" i

  | VFloat f ->
      pp_float ppf f

  | VBool b ->
      fprintf ppf "%b" b

  | VString s ->
      fprintf ppf "%S" s

  | VTuple values ->
      pp_value_tuple ppf values

  | VClosure _ ->
      fprintf ppf "<fun>"

and pp_value_tuple ppf values =
  match values with
  | [] ->
      fprintf ppf "()"

  | [value] ->
      fprintf ppf "(%a,)" pp_value value

  | values ->
      fprintf ppf
        "(@[<hov>%a@])"
        (pp_print_list
           ~pp_sep:(fun ppf () -> fprintf ppf ",@ ")
           pp_value)
        values

let value_to_string value =
  asprintf "%a" pp_value value
