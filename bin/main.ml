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

let interpret = flags "interpret sources" "interpret" 'i'

let () = match P.parse_argv op with
  | [] | _ :: _ :: _ -> P.usage op ()
  | [fname] -> 
    if O.get interpret then 
      match interpret_defs (parse (File.open_in fname)) with
      | IntVal i -> print_endline (string_of_int i)
      | _ -> ()
    else print_ctx (check fname)
