open OUnit2
open Interpreter.Lexer
open Parser
open Eval
open TypeDefinitions.PythonTypes

let capture_output f : string =
  let (stdout_old) = Unix.dup Unix.stdout in
  let output_channel = open_out "output_channel.txt" in
  Unix.dup2 (Unix.descr_of_out_channel output_channel) Unix.stdout;
  f ();
  flush stdout;
  Unix.dup2 stdout_old Unix.stdout;
  close_out output_channel;
  let input_channel = open_in "output_channel.txt" in
  let captured_output = really_input_string input_channel (in_channel_length input_channel) in
  close_in input_channel;
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