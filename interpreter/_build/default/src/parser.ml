open TypeDefinitions.PythonTypes
open TypeDefinitions.TokenTypes
open Utils.ParserUtils

let rec parse tokens stmts =
  let (tokens', stmt) = parse_stmt tokens in
  match lookahead tokens' with
  | None -> (tokens',stmt::stmts)
  | Some Token_EndLine -> 
    let tokens'' = match_token tokens' (Token_EndLine) in
    if tokens'' = [] then (tokens'',stmt::stmts) else parse tokens'' (stmt::stmts)
  | _ -> raise (InvalidInputException("Syntax"))

and parse_stmt tokens = 
  parse_assignmentStmt tokens

and parse_assignmentStmt tokens = 
  match lookahead_many tokens 2 with
  | Some [Token_Id id; Token_Assignment]->
    let t = match_token tokens (Token_Id id) in
    let t' = match_token t Token_Assignment in
    let (t'', stmt) = parse_additiveStmt t' in
    (t'', Assignment(id, stmt))
  | _ -> parse_functionCallStmt tokens

and parse_functionCallStmt tokens =
  match lookahead_many tokens 2 with
  | Some [Token_Id id; Token_LParen] -> 
    let t = match_token tokens (Token_Id id) in
    let t' = match_token t Token_LParen in
    let (t'', stmts) = parse_functionCallParameters t' in
    (t'', FunctionCall(id, stmts))
  | _ -> parse_additiveStmt tokens

and parse_functionCallParameters tokens =
  let (t, stmt) = parse_stmt tokens in
  match lookahead t with
  | Some Token_RParen -> 
    let t' = match_token t Token_RParen in
    (t', [stmt])
  | Some Token_Comma ->
    let t' = match_token t Token_Comma in
    let (t'', stmts) = parse_functionCallParameters t' in
    (t'', stmt::(stmts))
  | _ -> raise (InvalidInputException("Function parameter error"))

  and parse_additiveStmt tokens =
  let (t, multiplicativeStmt) = parse_multiplicativeStmt tokens in
  match lookahead t with
    | Some Token_Add ->
      let t' = match_token t Token_Add in
      let (t'', additiveStmt) = parse_additiveStmt t' in
      (t'', BinOp(Add, multiplicativeStmt, additiveStmt))
    | Some Token_Subtract ->
      let t' = match_token t Token_Subtract in
      let (t'', additiveStmt) = parse_additiveStmt t' in
      (t'', BinOp(Subtract, multiplicativeStmt, additiveStmt))
    | _ -> (t, multiplicativeStmt)

and parse_multiplicativeStmt tokens =
  let (t, primaryStmt) = parse_primaryStmt tokens in
  match lookahead t with
    | Some Token_Multiply ->
      let t' = match_token t Token_Multiply in
      let (t'', multiplicativeStmt) = parse_multiplicativeStmt t' in
      (t'', BinOp(Multiply, primaryStmt, multiplicativeStmt))
    | Some Token_Divide ->
      let t' = match_token t Token_Divide in
      let (t'', multiplicativeStmt) = parse_multiplicativeStmt t' in
      (t'', BinOp(Divide, primaryStmt, multiplicativeStmt))
    | _ -> (t, primaryStmt)

and parse_primaryStmt toks =
  match lookahead toks with
  | Some Token_Integer num -> let t = match_token toks (Token_Integer num) in (t, Object(Int(num)))
  | Some Token_Id id -> let t = match_token toks (Token_Id id) in (t, Var(id))
  | Some Token_LParen -> 
    let t = match_token toks Token_LParen in
    let (t', s) = parse_stmt t in
    let t'' = match_token t' Token_RParen in
    (t'', s)
  | Some s -> raise (InvalidInputException((string_of_token s)))
  | _ -> raise (InvalidInputException("badd"))

let parse_wrapper tokens =
  let (tokens, stmts) = parse tokens [] in
  let stmts' = List.rev stmts in (tokens, stmts')