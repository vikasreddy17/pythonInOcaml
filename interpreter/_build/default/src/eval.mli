open PythonTypes

exception DivByZeroError

val eval: stmt -> environment -> stmt * environment