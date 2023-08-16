open TypeDefinitions.PythonTypes

type functionsNative = Print | NotInBuilt

let native_print (params : stmt list) (env : environment) : stmt * environment =
  match params with
  | [] -> print_endline ""; (Object(None), env)
  | stmt::_ -> 
    match stmt with
    | Object(Int(num)) -> print_endline (string_of_int num); (Object(None), env)
    | Object(Float(num)) ->
      let s1 = string_of_float num in
      let s2 = if String.length s1 > 0 && String.get s1 (String.length s1 - 1) = '.' then s1 ^ "0" else s1 in
      print_endline s2; (Object(None), env)
    | Object(Boolean(1)) -> print_endline "True"; (Object(None), env)
    | Object(Boolean(0)) -> print_endline "False"; (Object(None), env)
    | Object(None) -> print_endline "None"; (Object(None), env)
    | _ -> (Object(None), env)


  
