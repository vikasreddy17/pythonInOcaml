open TypeDefinitions.PythonTypes
open TypeDefinitions.TokenTypes
open Utils.ParserUtils

let rec parse_simpleStmt tokens = 
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
  match lookahead_many tokens 3 with
  | Some [Token_Id id; Token_LParen; Token_RParen] ->
    let t = match_many tokens [(Token_Id id); Token_LParen; Token_RParen] in
    (t, FunctionCall(id, []))
  | Some [Token_Id id; Token_LParen; _] -> 
    let t = match_many tokens [(Token_Id id); Token_LParen] in
    let (t', stmts) = parse_functionCallParameters t in
    (t', FunctionCall(id, stmts))
  | _ -> parse_additiveStmt tokens

and parse_functionCallParameters tokens =
  let (t, param) = parse_simpleStmt tokens in
  match lookahead t with
  | Some Token_RParen -> 
    let t' = match_token t Token_RParen in
    (t', [param])
  | Some Token_Comma ->
    let t' = match_token t Token_Comma in
    let (t'', params) = parse_functionCallParameters t' in
    (t'', param::(params))
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
  | Some Token_Id id -> let t = match_token toks (Token_Id id) in (t, Var(id))
  | Some Token_Integer num -> let t = match_token toks (Token_Integer num) in (t, Object(Int(num)))
  | Some Token_Float num -> let t = match_token toks (Token_Float num) in (t, Object(Float(num)))
  | Some Token_Boolean b -> let t = match_token toks (Token_Boolean b) in (t, Object(Boolean(b)))
  | Some Token_None -> let t = match_token toks (Token_None) in (t, Object(None))
  | Some Token_LParen -> 
    let t = match_token toks Token_LParen in
    let (t', s) = parse_simpleStmt t in
    let t'' = match_token t' Token_RParen in
    (t'', s)
  | Some Token_EndLine -> (toks, Object(None))
  | _ -> raise (InvalidInputException("badd"))