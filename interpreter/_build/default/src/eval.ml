open PythonTypes

exception DivByZeroError

let rec eval e env = 
  match e with
  | Object(obj) -> eval_value obj env
  | Assignment(id, e') -> 
    let (obj, env) = eval e' env in
    Hashtbl.add env id obj;
    (obj, env)
  | BinOp(op, e1, e2) -> eval_binop op e1 e2 env

and eval_value obj env = 
  match obj with
  | Int(i) -> (Int(i), env)

and eval_binop op e1 e2 env =
  match op, (eval e1 env), (eval e2 env) with
  | Add, (Int(i1), _), (Int(i2), _) -> (Int(i1 + i2), env)
  | Subtract, (Int(i1), _), (Int(i2), _) -> (Int(i1 - i2), env)
  | Multiply, (Int(i1), _), (Int(i2), _) -> (Int(i1 * i2), env)
  | Divide, (Int(i1), _), (Int(i2), _) -> if i2 = 0 then raise DivByZeroError else (Int(i1 / i2), env)