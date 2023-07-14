open TypeDefinitions.PythonTypes

exception DivByZeroError

val eval: stmt list -> environment -> stmt * environment
val eval_stmt: stmt -> environment -> stmt * environment