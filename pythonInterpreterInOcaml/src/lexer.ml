open TokenTypes

let tokenize input =
  let length = String.length input in

  let rec tokenize_helper index =
    if index >= length then
      []

    else if Str.string_match (Str.regexp "[0-9]+") input index then
      let value = Str.matched_string input in
      let shift = String.length value in
      let num = int_of_string value in
      Token_Integer num::(tokenize_helper (index + shift))

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

    else if Str.string_match (Str.regexp "[a-zA-Z][a-zA-Z0-9]*") input index then
      let value = Str.matched_string input in
      let shift = String.length value in
      Token_Id value::(tokenize_helper (index + shift))

    else
      tokenize_helper (index + 1)

  in tokenize_helper 0