open TypeDefinitions.TokenTypes
open Utils.ParserUtils
open ParserSimpleStmt

let rec parse tokens stmts =
  let (tokens', stmt) = parse_simpleStmt tokens in
  match lookahead tokens' with
  | None -> (tokens',stmt::stmts)
  | Some Token_EndLine -> 
    let tokens'' = match_token tokens' (Token_EndLine) in
    if tokens'' = [] then (tokens'', stmt::stmts) else parse tokens'' (stmt::stmts)
  | Some s -> raise (InvalidInputException(string_of_token s))

and parse_stmt tokens = 
  parse_simpleStmt tokens

let parse_wrapper tokens =
  let (tokens, stmts) = parse tokens [] in
  let stmts' = List.rev stmts in (tokens, stmts')