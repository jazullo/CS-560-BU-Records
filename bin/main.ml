open! Batteries
open! Brml
open! Ast

let () = let lexbuf = Lexing.from_string "def main = 19 $" in
  let e = Parse.program_file Lex.token lexbuf in
  let a, _ = List.hd e in
  let _, _, b = a in
  let c, _, _ = b in
  assert (c = IntLit 19)
