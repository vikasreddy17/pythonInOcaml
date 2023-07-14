open Interpreter

let arguments = Sys.argv

let read_file (filename : string) : string =
  let channel = open_in filename in
  let rec read_lines acc =
    try
      let line = input_line channel in
      read_lines (acc ^ line ^ "\n")
    with
    | End_of_file -> acc
  in
  let file_content = read_lines "" in
  close_in channel;
  file_content

let run (filename : string) =
  let env = Hashtbl.create 10 in
  let (_, expr) = Parser.parse_wrapper (Lexer.tokenize (read_file filename)) in Eval.eval expr env;; 

run arguments.(1);

