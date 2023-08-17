open TypeDefinitions.PythonTypes

type functionsNative = Print | Abs | NotInBuilt

let rec native_print (params : stmt list) (env : environment) (count : int) : stmt * environment =
  if count > 0 then print_string " ";
  match params with
  | [] -> print_endline ""; (Object(None), env)
  | stmt::t -> 
    match stmt with
    | Object(Int(num)) -> print_string (string_of_int num); native_print t env (count+1)
    | Object(Float(num)) ->
      let s1 = string_of_float num in
      let s2 = if String.length s1 > 0 && String.get s1 (String.length s1 - 1) = '.' then s1 ^ "0" else s1 in
      print_string s2; native_print t env (count+1)
    | Object(Boolean(1)) -> print_string "True"; native_print t env (count+1)
    | Object(Boolean(0)) -> print_string "False"; native_print t env (count+1)
    | Object(None) -> print_string "None"; native_print t env (count+1)
    | _ -> (Object(None), env)

let native_abs (params : stmt list) (env : environment) : stmt * environment =
  if List.length params != 1 then raise (TypeError(
    "abs() takes exactly one argument (" ^ string_of_int (List.length params) ^ " given)"))
  else
    match params with
    | [Object(Int(i))] -> (Object(Int(abs i)), env)
    | [Object(Float(i))] -> (Object(Float(abs_float i)), env)
    | [Object(Boolean(i))] -> (Object(Int(abs i)), env)
    | _ -> raise (TypeError("Expected numeric type"))


  
