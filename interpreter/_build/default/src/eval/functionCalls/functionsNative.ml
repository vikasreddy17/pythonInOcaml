open TypeDefinitions.PythonTypes

type functionsNative = Print | Abs | NotInBuilt

let rec native_print (params : stmt list) (env : environment) (count : int) (str : string) : stmt * environment =
  let str' = if count > 0 then str ^ " " else str in
  match params with
  | [] -> print_endline (String.trim str'); (Object(None), env)
  | stmt::t -> 
    match stmt with
    | Object(Int(num)) -> let str'' = str' ^ (string_of_int num) in native_print t env (count+1) str''
    | Object(Float(num)) ->
      let s1 = string_of_float num in
      let s2 = if String.length s1 > 0 && String.get s1 (String.length s1 - 1) = '.' then s1 ^ "0" else s1 in
      let str'' = str' ^ s2 in native_print t env (count+1) str''
    | Object(Boolean(1)) -> let str'' = str' ^ "True" in native_print t env (count+1) str''
    | Object(Boolean(0)) -> let str'' = str' ^ "False" in native_print t env (count+1) str''
    | Object(None) -> let str'' = str' ^ "None" in native_print t env (count+1) str''
    | _ -> print_endline (String.trim str'); (Object(None), env)

let native_abs (params : stmt list) (env : environment) : stmt * environment =
  if List.length params != 1 then raise (TypeError(
    "abs() takes exactly one argument (" ^ string_of_int (List.length params) ^ " given)"))
  else
    match params with
    | [Object(Int(i))] -> (Object(Int(abs i)), env)
    | [Object(Float(i))] -> (Object(Float(abs_float i)), env)
    | [Object(Boolean(i))] -> (Object(Int(abs i)), env)
    | _ -> raise (TypeError("Expected numeric type"))


  
