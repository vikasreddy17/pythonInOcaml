type op = Add | Subtract | Multiply | Divide

and id = string

and obj =
  | None
  | Int of int

and environment = (id, stmt) Hashtbl.t

and stmt =
  | Var of id
  | Object of obj
  | Assignment of id * stmt
  | BinOp of op * stmt * stmt
  | FunctionCall of id * stmt list