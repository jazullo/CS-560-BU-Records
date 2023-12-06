open! Batteries
open! Brml
open! Interpreter

let () = let lexbuf = Lexing.from_string "def main = 10 $" in
  let program = Parse.program_file Lex.token lexbuf in
  match interpret_defs program with
    | IntVal i -> print_endline (string_of_int i)
    | _ -> ()
