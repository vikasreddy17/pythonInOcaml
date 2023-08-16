# Python Interpreter Written in Ocaml

## Features Implemented

### CLI and File IO
* Running files directly in CLI with the following command
    ```
        sh interp_python.sh <path>
    ```
* Reading and running multi line files

### Variables 
* Assignment of variables. Saves variables in environment variable as a hastable

### Data Types 
* Integers
    * Printing
    * Addition, Subtraction, Multiplication, Division
        * Compatible operations with Booleans and Floats
* Booleans
    * Printing
    * Performing "+ - * /" on booleans
        * Compatible operations with Booleans and Floats
* Floats
    * Printing
    * Addition, Subtraction, Multiplication, Division
        * Compatible operations with Booleans and Ints
* None
    * Printing

### Functions
* Storing parameters as list to pass to function call architecture
* Function call router for in built functions

#### In Built Functions
* print(a, b, c, ...)
    * Takes list of parameters and prints them with space as separator
* abs(a)
    * Takes absoulte values of parameter
    * a must be a float or a int
    * NOT abtractable to any object implementing `__abs__()`


## To do
* Implement PEMDAS
* More In-Built Python Functions [List of Native Functions in Python](https://docs.python.org/3/library/functions.html)