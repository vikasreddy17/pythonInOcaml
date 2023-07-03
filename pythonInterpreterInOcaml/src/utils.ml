open TokenTypes

let string_of_token (t : token) : string = match t with
  | Token_Add -> "Token_Add"
  | Token_Subtract -> "Token_Subtract"
  | Token_Multiply -> "Token_Multiply"
  | Token_Divide -> "Token_Divide"
  | Token_LParen -> "Token_LParen"
  | Token_RParen -> "Token_RParen"
  | Token_Integer(i) -> "Token_Integer(" ^ (string_of_int i) ^ ")"

let string_of_list ?newline:(newline=false) (f : 'a -> string) (l : 'a list) : string =
  "[" ^ (String.concat ", " @@ List.map f l) ^ "]" ^ (if newline then "\n" else "");;

let match_token (toks: token list) (tok: token) =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)))

let lookahead (toks: token list) = 
  match toks with
  | [] -> None
  | h::_ -> Some h