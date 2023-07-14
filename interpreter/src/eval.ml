open TypeDefinitions.PythonTypes
open FunctionCalls.FunctionRouter

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
  | None -> (Object(None), env)

and eval_functionCall id params env =
  let rec eval_functionParams ps e =
    match ps with
    | [] -> []
    | h::t -> let (obj, _) = (eval h e) in obj::(eval_functionParams t e)
  in
  function_router id (eval_functionParams params env) env
    
and eval_binOp op e1 e2 env =
  match op, (eval e1 env), (eval e2 env) with
  | Add, (Object(Int(i1)), _), (Object(Int(i2)), _) -> (Object(Int(i1 + i2)), env)
  | Subtract, (Object(Int(i1)), _), (Object(Int(i2)), _) -> (Object(Int(i1 - i2)), env)
  | Multiply, (Object(Int(i1)), _), (Object(Int(i2)), _) -> (Object(Int(i1 * i2)), env)
  | Divide, (Object(Int(i1)), _), (Object(Int(i2)), _) -> if i2 = 0 then raise DivByZeroError else (Object(Int(i1 / i2)), env)
  | _, _, _ -> raise (TypeError("Cannot compare types"))