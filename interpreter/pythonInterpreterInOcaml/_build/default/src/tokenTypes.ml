exception InvalidInputException of string

type token =
  | Token_Add
  | Token_Subtract
  | Token_Multiply
  | Token_Divide
  | Token_Integer of int