open Ast
open Error

let rec lookup name env = 
  match env with
  | [] -> raise_error (VarError name)
  | (k, v) :: rest -> if k = name then !v else lookup name rest

let as_int = function
  | VInt i -> i
  | v -> raise_error (TypeError(TInt, val_to_type v))

let as_bool = function 
  | VBool b -> b
  | v -> raise_error (TypeError(TBool, val_to_type v))

let as_float = function 
  | VFloat f -> f
  | v -> raise_error (TypeError(TFloat, val_to_type v)) 

let int_op a b abinop = 
  match abinop with 
  | Add -> VInt(a + b)
  | Sub -> VInt(a - b)
  | Mul -> VInt(a * b)
  | Div -> if b = 0 then raise_error(DivByZeroError) else VInt(a / b)

let float_op a b fbinop =
  match fbinop with 
  | Add -> VFloat(a +. b)
  | Sub -> VFloat(a -. b)
  | Mul -> VFloat(a *. b)
  | Div -> if b = 0. then raise_error(DivByZeroError) else VFloat(a /. b)

let arithmetic a b binop = match a, b with 
  | VInt i, VInt j -> int_op i j binop
  | VFloat i, VFloat j -> float_op i j binop
  | _ -> raise_error(TypeError(val_to_type a, val_to_type b))

let comparison a b bbinop =
match bbinop with 
  | Eq -> VBool (a = b)
  | Le -> VBool(a <= b)
  | Lt -> VBool(a < b)
  | Ge -> VBool(a >= b)
  | Gt -> VBool(a > b)

let strict_comparison a b binop = match a, b with 
  | VInt i, VInt j -> comparison a b binop
  | VFloat i, VFloat j -> comparison a b binop
  | _ -> raise_error(TypeError(val_to_type a, val_to_type b))

let rec match_eval pat v = match pat, v with
  | PWild, _ -> Some []
  | PVar pat, v -> Some [(pat, v)]
  | PInt pat, VInt v -> if pat = v then Some [] else None
  | PFloat pat, VFloat v -> if pat = v then Some [] else None
  | PBool pat, VBool v -> if pat = v then Some [] else None
  | PTuple pat, VTuple v -> tuple_check pat v
  | _, _ -> None

and tuple_check pats vals = match pats, vals with 
  | [], [] -> Some []
  | p :: ps, v :: vs -> (
    let eval_bind = (match_eval p v) in match eval_bind with 
    | None -> None
    | Some bindings -> let r_check = tuple_check ps vs 
    in match r_check with
      | None -> None
      | Some t_bindings -> Some (bindings @ t_bindings))
  | _ -> None


let rec eval env = function
  | Int i -> VInt i
  | Float f -> VFloat f
  | Var s -> lookup s env
  | Arith (l, op, r) -> let i = eval env l in let j = eval env r in arithmetic i j op
  | Comp (l, op, r) -> let i = eval env l in let j = eval env r in strict_comparison i j op
  | Let (var, bind, body) -> 
    let bounded_val = eval env bind in
    let new_env = (var, ref bounded_val) :: env in
    eval new_env body
  | LetRec(var, bind, body) -> let dummy = ref (VInt 0) in 
    let new_env = (var, dummy) :: env in let r_closure = eval new_env bind in
    dummy := r_closure; eval new_env body
  | Bool b -> VBool b
  | If (init_cond, t_branch, f_branch) -> let eval_cond = eval env init_cond in
    (match eval_cond with
    | VBool true -> eval env t_branch
    | VBool false -> eval env f_branch
    | v -> raise_error (TypeError(TBool, val_to_type v))
      )
  | Fun (param, body) -> VClosure(param, body, env)
  | App (fn, arg) -> let eval_fn = eval env fn in 
  let eval_arg = eval env arg in (
    match eval_fn with 
  | VClosure(param, body, captured_env) -> let new_env = (param, ref eval_arg) :: captured_env in eval new_env body
  | v -> raise_error (AppError (val_to_type v))
  ) (* App case: invalid function application or wrong closure construction *)
  | Tuple tp_lst -> VTuple(List.map (eval env) tp_lst)
  | Match (v_mat, pt_lst) -> let v_m = eval env v_mat in matcher v_m env pt_lst

and matcher value env branches = 
  match branches with 
  | [] -> raise_error (MatchError(val_to_type value))
  | (pat, body) :: rest -> let eval_match = match_eval pat value in
    match eval_match with 
    | None -> matcher value env rest
    | Some bindings -> let env_entry = List.map(fun(name, v) -> (name, ref v)) bindings 
      in eval (env_entry @ env) body