open TypeDefinitions.PythonTypes
open TypeDefinitions.TokenTypes

val parse_wrapper : token list -> (token list * stmt list)
val parse : token list -> stmt list -> (token list * stmt list)
val parse_stmt : token list -> (token list * stmt)