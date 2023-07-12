open PythonTypes
open TokenTypes
open Utils

let rec parse tokens = 
  parse_assignmentExpr tokens

and parse_assignmentExpr tokens = 
  match lookahead tokens with
  | Some Token_Id(id) -> 
    let t = match_token tokens (Token_Id id) in
    let t' = match_token t Token_Assignment in
    let (t'', expr) = parse_mathExpr t' in
    
    (t'', Assignment(id, expr))
  | _ -> parse_mathExpr tokens

and parse_mathExpr tokens =
  let (t, primaryExpr) = parse_primaryExpr tokens in
  match lookahead t with
  | Some Token_Add ->
    let t' = match_token t Token_Add in
    let (t'', additiveExpr) = parse_mathExpr t' in
    (t'', BinOp(Add, primaryExpr, additiveExpr))
  | Some Token_Subtract ->
    let t' = match_token t Token_Subtract in
    let (t'', additiveExpr) = parse_mathExpr t' in
    (t'', BinOp(Subtract, primaryExpr, additiveExpr))
  | Some Token_Multiply ->
    let t' = match_token t Token_Multiply in
    let (t'', additiveExpr) = parse_mathExpr t' in
    (t'', BinOp(Multiply, primaryExpr, additiveExpr))
  | Some Token_Divide ->
    let t' = match_token t Token_Divide in
    let (t'', additiveExpr) = parse_mathExpr t' in
    (t'', BinOp(Divide, primaryExpr, additiveExpr))
  | _ -> (t, primaryExpr)

and parse_primaryExpr toks =
  match lookahead toks with
  | Some Token_Integer num -> let t = match_token toks (Token_Integer num) in (t, Object(Int(num)))
  | Some Token_LParen -> 
    let t = match_token toks Token_LParen in
    let (t', s) = parse t in
    let t'' = match_token t' Token_RParen in
    (t'', s)
  | _ -> raise (InvalidInputException("Parse error"))