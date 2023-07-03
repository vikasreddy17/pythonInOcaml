open PythonTypes

exception DivByZeroError

let rec eval e env = 
  match e with
  | Value(v) -> eval_value v
  | Binop(op, e1, e2) -> eval_binop op e1 e2 env

and eval_value v = 
  match v with
  | Int(i) -> Int i

and eval_binop op e1 e2 env=
  match op, (eval e1 env), (eval e2 env) with
  | Add, Int(i1), Int(i2) -> Int(i1 + i2)
  | Subtract, Int(i1), Int(i2) -> Int(i1 - i2)
  | Multiply, Int(i1), Int(i2) -> Int(i1 * i2)
  | Divide, Int(i1), Int(i2) -> if i2 = 0 then raise DivByZeroError else Int(i1 / i2)