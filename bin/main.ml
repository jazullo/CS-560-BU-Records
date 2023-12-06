[@@@warning "-32"]
[@@@warning "-33"]

open! Batteries
open! Brml
open! Interpreter
open! Ast
open Printf
open Cli


let parse ch = 
  let lexbuf = Lexing.from_channel ch in
  Parse.program_file Lex.token lexbuf

let check fname = 
  let ast = parse (File.open_in fname) in
  System.infer_defs Cyclic.empty ast

let print_ctx ctx = 
  Cyclic.to_list ctx |> List.iter @@ fun (name, ty) -> 
    printf "%s : " name;
    Types.Show.print_ty stdout ty;
    printf "\n"

(* let interpret = flags "interpret sources" "interpret" 'i' *)
(* if O.get interpret then ... *)

(* let () = match P.parse_argv op with
  | [] | _ :: _ :: _ -> P.usage op ()
  | [fname] -> print_ctx (check fname) *)

  

let () = let lexbuf = Lexing.from_string "def main = 10 $" in
let program = Parse.program_file Lex.token lexbuf in
match interpret_defs program with
  | IntVal i -> print_endline (string_of_int i)
  | _ -> ()
