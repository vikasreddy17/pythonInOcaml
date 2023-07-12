type op = Add | Subtract | Multiply | Divide

and obj =
  | Int of int

and id = string

and environment = (id, obj) Hashtbl.t

and expr =
  | Object of obj
  | Assignment of id * expr
  | BinOp of op * expr * expr