open TypeDefinitions.PythonTypes
open FunctionsNative

let function_token_of_string (f_id : string) : functionsNative = 
  match f_id with
  | "print"  -> Print
  | "abs"  -> Abs
  | _ -> NotInBuilt

let function_router (id : id) (params : stmt list) (env : environment): stmt * environment = 
  match function_token_of_string id with
  | Print -> native_print params env 0
  | Abs -> native_abs params env
  | NotInBuilt -> (Object(None), env)

