open TypeDefinitions.TokenTypes

let string_of_token (t : token) : string = 
  match t with
  | Token_Add -> "Token_Add"
  | Token_Subtract -> "Token_Subtract"
  | Token_Multiply -> "Token_Multiply"
  | Token_Divide -> "Token_Divide"
  | Token_LParen -> "Token_LParen"
  | Token_RParen -> "Token_RParen"
  | Token_Comma -> "Token_Comma"
  | Token_Integer(i) -> "Token_Integer(" ^ (string_of_int i) ^ ")"
  | Token_None -> "Token_None"
  | Token_Id(id) -> "Token_Id(" ^ (id) ^ ")"
  | Token_Assignment -> "Token_Assignment"

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

let match_many (toks: token list) (to_match: token list) =
  List.fold_left match_token toks to_match

let lookahead (toks: token list) = 
  match toks with
  | [] -> None
  | h::_ -> Some h

let rec lookahead_many (toks: token list) (n: int) : 'a list option =
  match n, toks with
  | _, _ when n <= 0 -> Some []
  | 0, _ -> Some []
  | _, [] -> None
  | n, x :: xs ->
    match lookahead_many xs (n-1) with
    | Some result -> Some (x :: result)
    | None -> None
