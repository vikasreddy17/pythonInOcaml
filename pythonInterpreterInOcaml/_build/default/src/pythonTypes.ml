type op = Add | Subtract | Multiply | Divide

and value =
  | Int of int

and var = string

and environment = (var * value ref) list

and expr =
  | Value of value
  | Binop of op * expr * expr