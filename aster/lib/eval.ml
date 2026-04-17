open Ast

let rec lookup (name: string) (env: env) = 
  match env with
  | [] -> failwith "There needs to be an existent variable in the environment!"
  | (k, v) :: rest -> if k = name then !v else lookup name rest

let as_int = function
  | VInt i -> i
  | _ -> failwith "Not a valid integer value."

let as_bool = function 
  | VBool b -> b
  | _ -> failwith "Not a valid boolean value."

let as_float = function 
  | VFloat f -> f
  | _ -> failwith "Not a valid floating point value"

let int_op (a: int) (b: int) abinop = 
match abinop with 
| Add -> VInt(a + b)
| Sub -> VInt(a - b)
| Mul -> VInt(a * b)
| Div -> VInt(a / b)
|  _ -> failwith "Not valid integer operations."

let float_op (a: float) (b: float) fbinop =
match fbinop with 
| Add -> VFloat(a +. b)
| Sub -> VFloat(a -. b)
| Mul -> VFloat(a *. b)
| Div -> VFloat(a /. b)
| _ -> failwith "Not valid floating point operations."

let arithmetic a b binop = match a, b with 
  | VInt i, VInt j -> int_op i j binop
  | VFloat i, VFloat j -> float_op i j binop
  | _ -> failwith "Invalid operation." 

let comparison a b bbinop =
match bbinop with 
| Eq -> VBool (a = b)
| Le -> VBool(a <= b)
| Lt -> VBool(a < b)
| Ge -> VBool(a >= b)
| Gt -> VBool(a > b)
| _ -> failwith "You need to use comparison operators for this to work."

let strict_comparison a b binop = match a, b with 
  | VInt i, VInt j -> comparison i j binop
  | VFloat i, VFloat j -> comparison i j binop
  | _ -> failwith "Type Error: Comparison is only allowed between NUMERIC values."  

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
  | Binop (l, bop, r) -> let i = eval env l 
  in let j = eval env r in (
    match bop with 
    | Add | Sub | Mul | Div -> arithmetic i j bop
    | Eq | Le | Lt | Ge | Gt -> strict_comparison i j bop
  )
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
    | _ -> failwith "This is not a valid if construct!"
      )
  | Fun (param, body) -> VClosure(param, body, env)
  | App (fn, arg) -> let eval_fn = eval env fn in 
  let eval_arg = eval env arg in (
    match eval_fn with 
  | VClosure(param, body, captured_env) -> let new_env = (param, ref eval_arg) :: captured_env in eval new_env body
  | _ -> failwith "Not a valid function!"
  )
  | Tuple tp_lst -> VTuple(List.map (eval env) tp_lst)
  | Match (v_mat, pt_lst) -> failwith "todo"