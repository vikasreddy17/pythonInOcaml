open TypeDefinitions.PythonTypes

type functionsNative = Print | NotInBuilt

let native_print (params : stmt list) (env : environment) : stmt * environment =
  match params with
  | [] -> print_endline ""; (Object(None), env)
  | stmt::_ -> 
    match stmt with
    | Object(Int(num)) -> print_endline (string_of_int num); (Object(None), env)
    | Object(None) -> print_endline "None"; (Object(None), env)
    | _ -> (Object(None), env)


  
