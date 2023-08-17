open OUnit2
open Interpreter.Lexer
open Parser
open Eval
open TypeDefinitions.PythonTypes

let capture_output f : string =
  let (stdout_orig : Unix.file_descr) = Unix.dup Unix.stdout in
  let temp_file = open_out "output_capture.txt" in
  Unix.dup2 (Unix.descr_of_out_channel temp_file) Unix.stdout;
  f ();
  flush stdout;
  Unix.dup2 stdout_orig Unix.stdout;
  close_out temp_file;
  let ic = open_in "output_capture.txt" in
  let captured_output = really_input_string ic (in_channel_length ic) in
  close_in ic;
  captured_output

let test_none _ =
  let input = "None" in
  let env = Hashtbl.create 10 in 
  let (_, stmts) = parse_wrapper (tokenize input) in 
  let (result, _) = eval stmts env in
  let expected = Object(None) in
  assert_equal expected result ~msg:"test_none"

let test_int_math _ =
  let input = "print((1+2) - (4*6))" in
  let result = capture_output (fun () ->
    let env = Hashtbl.create 10 in 
    let (_, stmts) = parse_wrapper (tokenize input) in 
    let (_, _) = eval stmts env in () ) in
  let expected = "-21\n" in
  assert_equal expected result ~msg:"test_int_math"

let suite = 
  "tests" >::: [
    "test_none" >:: test_none;
    "test_int_math" >:: test_int_math;
  ]
let _ = run_test_tt_main suite