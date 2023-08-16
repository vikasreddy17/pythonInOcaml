open TypeDefinitions.PythonTypes
open FunctionCalls.FunctionRouter

exception TypeError of string
exception DivByZeroError


let rec eval stmts env =
  match stmts with
  | [] -> (Object(None), env)
  | h::t -> 
    let (_, env') = eval_stmt h env in
    eval t env'

and eval_stmt stmt env = 
  match stmt with
  | Var(id) -> 
    let obj = Hashtbl.find env id in
    (obj, env)
  | Object(obj) -> eval_value obj env
  | Assignment(id, e') -> 
    let (obj, env) = eval_stmt e' env in
    Hashtbl.add env id obj;
    (obj, env)
  | FunctionCall(id, params) -> eval_functionCall id params env
  | BinOp(op, e1, e2) -> eval_binOp op e1 e2 env

and eval_value obj env = 
  match obj with
  | Int(i) -> (Object(Int(i)), env)
  | Float(i) -> (Object(Float(i)), env)
  | Boolean(b) -> (Object(Boolean(b)), env)
  | None -> (Object(None), env)

and eval_functionCall id params env =
  let rec eval_functionParams ps e =
    match ps with
    | [] -> []
    | h::t -> let (obj, _) = (eval_stmt h e) in obj::(eval_functionParams t e)
  in
  function_router id (eval_functionParams params env) env
    
and eval_binOp op e1 e2 env =
  
  match op, (eval_stmt e1 env), (eval_stmt e2 env) with

  | Add, (Object(Boolean(i1)), _), (Object(Boolean(i2)), _) -> (Object(Int(i1 + i2)), env)
  | Subtract, (Object(Boolean(i1)), _), (Object(Boolean(i2)), _) -> (Object(Int(i1 - i2)), env)
  | Multiply, (Object(Boolean(i1)), _), (Object(Boolean(i2)), _) -> (Object(Int(i1 * i2)), env)
  | Divide, (Object(Boolean(i1)), _), (Object(Boolean(i2)), _) -> if i2 = 0 then raise DivByZeroError else (Object(Int(i1 / i2)), env)

  | Add, (Object(Int(i1)), _), (Object(Int(i2)), _) -> (Object(Int(i1 + i2)), env)
  | Subtract, (Object(Int(i1)), _), (Object(Int(i2)), _) -> (Object(Int(i1 - i2)), env)
  | Multiply, (Object(Int(i1)), _), (Object(Int(i2)), _) -> (Object(Int(i1 * i2)), env)
  | Divide, (Object(Int(i1)), _), (Object(Int(i2)), _) -> if i2 = 0 then raise DivByZeroError else (Object(Int(i1 / i2)), env)

  | Add, (Object(Float(i1)), _), (Object(Float(i2)), _) -> (Object(Float(i1 +. i2)), env)
  | Subtract, (Object(Float(i1)), _), (Object(Float(i2)), _) -> (Object(Float(i1 -. i2)), env)
  | Multiply, (Object(Float(i1)), _), (Object(Float(i2)), _) -> (Object(Float(i1 *. i2)), env)
  | Divide, (Object(Float(i1)), _), (Object(Float(i2)), _) -> if i2 = 0.0 then raise DivByZeroError else (Object(Float(i1 /. i2)), env)

  | Add, (Object(Int(i1)), _), (Object(Boolean(i2)), _) -> (Object(Int(i1 + i2)), env)
  | Add, (Object(Boolean(i1)), _), (Object(Int(i2)), _) -> (Object(Int(i1 + i2)), env)
  | Subtract, (Object(Int(i1)), _), (Object(Boolean(i2)), _) -> (Object(Int(i1 - i2)), env)
  | Subtract, (Object(Boolean(i1)), _), (Object(Int(i2)), _) -> (Object(Int(i1 - i2)), env)
  | Multiply, (Object(Int(i1)), _), (Object(Boolean(i2)), _) -> (Object(Int(i1 - i2)), env)
  | Multiply, (Object(Boolean(i1)), _), (Object(Int(i2)), _) -> (Object(Int(i1 - i2)), env)
  | Divide, (Object(Int(i1)), _), (Object(Boolean(i2)), _) -> if i2 = 0 then raise DivByZeroError else (Object(Int(i1 / i2)), env)
  | Divide, (Object(Boolean(i1)), _), (Object(Int(i2)), _) -> if i2 = 0 then raise DivByZeroError else (Object(Int(i1 / i2)), env)

  | Add, (Object(Int(i)), _), (Object(Float(f)), _) -> (Object(Float(float_of_int i +. f)), env)
  | Add, (Object(Float(f)), _), (Object(Int(i)), _) -> (Object(Float(f +. float_of_int i )), env)
  | Subtract, (Object(Int(i)), _), (Object(Float(f)), _) -> (Object(Float(float_of_int i -. f)), env)
  | Subtract, (Object(Float(f)), _), (Object(Int(i)), _) -> (Object(Float(f -. float_of_int i )), env)
  | Multiply, (Object(Int(i)), _), (Object(Float(f)), _) -> (Object(Float(float_of_int i *. f)), env)
  | Multiply, (Object(Float(f)), _), (Object(Int(i)), _) -> (Object(Float(f *. float_of_int i )), env)
  | Divide, (Object(Int(i)), _), (Object(Float(f)), _) -> if f = 0.0 then raise DivByZeroError else (Object(Float(float_of_int i /. f)), env)
  | Divide, (Object(Float(f)), _), (Object(Int(i)), _) -> if i = 0 then raise DivByZeroError else (Object(Float(f /. float_of_int i)), env)

  | Add, (Object(Boolean(i)), _), (Object(Float(f)), _) -> (Object(Float(float_of_int i +. f)), env)
  | Add, (Object(Float(f)), _), (Object(Boolean(i)), _) -> (Object(Float(f +. float_of_int i )), env)
  | Subtract, (Object(Boolean(i)), _), (Object(Float(f)), _) -> (Object(Float(float_of_int i -. f)), env)
  | Subtract, (Object(Float(f)), _), (Object(Boolean(i)), _) -> (Object(Float(f -. float_of_int i )), env)
  | Multiply, (Object(Boolean(i)), _), (Object(Float(f)), _) -> (Object(Float(float_of_int i *. f)), env)
  | Multiply, (Object(Float(f)), _), (Object(Boolean(i)), _) -> (Object(Float(f *. float_of_int i )), env)
  | Divide, (Object(Boolean(i)), _), (Object(Float(f)), _) -> if f = 0.0 then raise DivByZeroError else (Object(Float(float_of_int i /. f)), env)
  | Divide, (Object(Float(f)), _), (Object(Boolean(i)), _) -> if i = 0 then raise DivByZeroError else (Object(Float(f /. float_of_int i)), env)


  | _, _, _ -> raise (TypeError("Cannot compare types"))