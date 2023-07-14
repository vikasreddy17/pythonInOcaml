open PythonTypes

exception TypeError of string
exception DivByZeroError

let rec eval stmt env = 
  match stmt with
  | Var(id) -> (Var(id), env)
  | Object(obj) -> eval_value obj env
  | Assignment(id, e') -> 
    let (obj, env) = eval e' env in
    Hashtbl.add env id obj;
    (obj, env)
  | FunctionCall(id, params) -> eval_functionCall id params env
  | BinOp(op, e1, e2) -> eval_binOp op e1 e2 env

and eval_value obj env = 
  match obj with
  | Int(i) -> (Object(Int(i)), env)

and eval_functionCall _ params env =
  match params with
  | [] -> print_endline ""; (Object(Int(0)), env)
  | stmt::_ -> 
    let (stmt', env') = (eval stmt env) in
    match stmt' with
    | Object(Int(num)) -> print_endline (string_of_int num); (stmt', env')
    | _ -> (stmt', env')
    
and eval_binOp op e1 e2 env =
  match op, (eval e1 env), (eval e2 env) with
  | Add, (Object(Int(i1)), _), (Object(Int(i2)), _) -> (Object(Int(i1 + i2)), env)
  | Subtract, (Object(Int(i1)), _), (Object(Int(i2)), _) -> (Object(Int(i1 - i2)), env)
  | Multiply, (Object(Int(i1)), _), (Object(Int(i2)), _) -> (Object(Int(i1 * i2)), env)
  | Divide, (Object(Int(i1)), _), (Object(Int(i2)), _) -> if i2 = 0 then raise DivByZeroError else (Object(Int(i1 / i2)), env)
  | _, _, _ -> raise (TypeError("Cannot compare types"))