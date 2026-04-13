open Ast

let rec lookup name env = 
  match env with
  | [] -> failwith "There needs to be an existent variable in the environment!"
  | (k, v) :: rest -> if k = name then v else lookup name rest

let as_int = function
  | VInt i -> i
  | _ -> failwith "Not a valid integer value."

let as_bool = function 
  | VBool b -> b
  | _ -> failwith "Not a valid boolean value."

let rec eval env = function
  | Int i -> VInt i
  | Var s -> lookup s env
  | Add (l, r) -> 
    (match eval env l, eval env r with
    | VInt i, VInt j -> VInt (i + j)
    | VInt i, VBool j -> failwith "You cannot multiply an integer and a boolean."
    | VBool i, VInt j -> failwith "You cannot multipli a bool with an integer."
    | VBool i, VBool j -> failwith "You cannot multiply two booleans.")
  | Sub (l, r) -> 
    (match eval env l, eval env r with
    | VInt i, VInt j -> VInt (i - j)
    | VInt i, VBool j -> failwith "Type Error: You cannot substract an integer and a boolean."
    | VBool i, VInt j -> failwith "Type Error: You cannot substract a bool with an integer."
    | VBool i, VBool j -> failwith "Type Error: You cannot substract two booleans.")
  | Mul (l, r) -> 
    (match eval env l, eval env r with
    | VInt i, VInt j -> VInt (i * j)
    | VInt i, VBool j -> failwith "You cannot multiply an integer and a boolean."
    | VBool i, VInt j -> failwith "You cannot multiply a bool with an integer."
    | VBool i, VBool j -> failwith "You cannot multiply two booleans."
    )
  | Div (l, r) -> 
    (match eval env l, eval env r with
    | VInt i, VInt j -> if j = 0 then 
      failwith "Division by zero" else VInt (i / j)
    | VInt i, VBool j -> failwith "You cannot divide an integer and a boolean."
    | VBool i, VInt j -> failwith "You cannot divide an integer and a boolean."
    | VBool i, VBool j -> failwith "You cannot divide two booleans."
    )
  | Let (var, bind, body) -> 
    let bounded_val = eval env bind in
    let new_env = (var, bounded_val) :: env in
    eval new_env body
  | Bool b -> VBool b
