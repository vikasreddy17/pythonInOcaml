open PythonTypes

exception InvalidInputException of string

type token =
  | Token_Add
  | Token_Subtract
  | Token_Multiply
  | Token_Divide
  | Token_LParen
  | Token_RParen
  | Token_Comma
  | Token_Integer of int
  | Token_Float of float
  | Token_Boolean of int
  | Token_None
  | Token_Id of id
  | Token_Assignment
  | Token_EndLine