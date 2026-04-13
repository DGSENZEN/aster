open Ast

type value =
  | VInt of int

type env = (string * value) list


let rec lookup name env = 
  match env with
  | [] -> failwith "There needs to be an existent variable in the environment!"
  | (k, v) :: rest -> if k = name then v else lookup name rest

let rec eval env = function
  | Int i -> VInt i
  | Var s -> lookup s env
  | Add (l, r) -> 
    (match eval env l, eval env r with
    | VInt i, VInt j -> VInt (i + j))
  | Sub (l, r) -> 
    (match eval env l, eval env r with
    | VInt i, VInt j -> VInt (i - j))
  | Mul (l, r) -> 
    (match eval env l, eval env r with
    | VInt i, VInt j -> VInt (i * j))
  | Div (l, r) -> 
    (match eval env l, eval env r with
    | VInt i, VInt j -> if j = 0 then failwith "Division by zero" else VInt (i / j))
  | Let (var, bind, body) -> 
    let bounded_val = eval env bind in
    let new_env = (var, bounded_val) :: env in
    eval new_env body