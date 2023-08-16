open TypeDefinitions.TokenTypes

let tokenize input =
  let length = String.length input in

  let rec tokenize_helper index =
    if index >= length then
      []

    else if Str.string_match (Str.regexp "[-]?[0]?\\.[0-9]+") input index then
      let value = Str.matched_string input in
      let shift = String.length value in
      let num = float_of_string value in
      Token_Float num::(tokenize_helper (index + shift))

    else if Str.string_match (Str.regexp "[-]?[0-9]+\\.[0-9]+") input index then
      let value = Str.matched_string input in
      let shift = String.length value in
      let num =  float_of_string ("0" ^ value) in
      Token_Float num::(tokenize_helper (index + shift))
    
    else if Str.string_match (Str.regexp "[-]?[0-9]+") input index then
      let value = Str.matched_string input in
      let shift = String.length value in
      let num = int_of_string value in
      Token_Integer num::(tokenize_helper (index + shift))

    else if Str.string_match (Str.regexp "\n") input index then
      Token_EndLine::(tokenize_helper (index + 1))

    else if Str.string_match (Str.regexp "None") input index then
      Token_None::(tokenize_helper (index + 4))

    else if Str.string_match (Str.regexp "True") input index then
      Token_Boolean 1::(tokenize_helper (index + 4))

    else if Str.string_match (Str.regexp "False") input index then
      Token_Boolean 0::(tokenize_helper (index + 5))

    else if Str.string_match (Str.regexp "=") input index then
      Token_Assignment::(tokenize_helper (index + 1))

    else if Str.string_match (Str.regexp "+") input index then
      Token_Add::(tokenize_helper (index + 1))

    else if Str.string_match (Str.regexp "-") input index then
      Token_Subtract::(tokenize_helper (index + 1))

    else if Str.string_match (Str.regexp "*") input index then
      Token_Multiply::(tokenize_helper (index + 1))

    else if Str.string_match (Str.regexp "/") input index then
      Token_Divide::(tokenize_helper (index + 1))

    else if Str.string_match (Str.regexp "(") input index then
      Token_LParen::(tokenize_helper (index + 1))

    else if Str.string_match (Str.regexp ")") input index then
      Token_RParen::(tokenize_helper (index + 1))

    else if Str.string_match (Str.regexp ",") input index then
      Token_Comma::(tokenize_helper (index + 1))

    else if Str.string_match (Str.regexp "[a-zA-Z][a-zA-Z0-9]*") input index then
      let value = Str.matched_string input in
      let shift = String.length value in
      Token_Id value::(tokenize_helper (index + shift))

    else
      tokenize_helper (index + 1)

  in tokenize_helper 0