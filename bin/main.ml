[@@@warning "-32"]
[@@@warning "-33"]

open! Batteries
open! Brml
(* open! Interpreter *)
open! Ast
open Printf
open Cli


open Printf

let rec print_prog out prog = 
  List.iter (fun ((s, e), _sp, t) -> 
    fprintf out "(Def %s " s;
    print_expr out e;
    fprintf out "%s" " : ";
    print_ty out t;
    fprintf out "%s" ")\n") prog

and print_ty o = Types.Show.print_ty o

and print_expr out (_e, _, t) = 
  fprintf out "(";
  begin match _e with
  | Ternary (e1, e2, e3) -> 
    fprintf out "If "; print_expr out e1;
    fprintf out " Then "; print_expr out e2;
    fprintf out " Else "; print_expr out e3
  | Apply (e1, e2) -> 
    print_expr out e1;
    fprintf out " ";
    print_expr out e2
  | Arithmetic (e1, op, e2) -> 
    print_expr out e1;
    fprintf out " %s " (match op with
      | Add -> "+" | Sub -> "-"
      | Mul -> "*" | Div -> "/" | Mod -> "mod");
    print_expr out e2
  | Comparative (e1, op, e2) -> 
    print_expr out e1;
    fprintf out " %s " (match op with
      | Eq -> "=" | Ne -> "<>"
      | Gt -> ">" | Lt -> "<" | Ge -> ">=" | Le -> "<=");
    print_expr out e2
  | Logical (e1, op, e2) -> 
    print_expr out e1;
    fprintf out " %s " (match op with And -> "&&" | Or -> "||");
    print_expr out e2
  | Not e -> fprintf out "!"; print_expr out e
  | Record (e1, op, e2) -> 
    print_expr out e1;
    fprintf out " %s " (match op with Concatenate -> "&" | Intersect -> "|");
    print_expr out e2
  | Project (e, s) -> 
    print_expr out e;
    fprintf out ".%s" s
  | Binding (s, e1, e2) -> 
    fprintf out "Let %s = " s;
    print_expr out e1;
    fprintf out " In ";
    print_expr out e2
  | Abstract (p, e) -> 
    fprintf out "Fun ";
    print_pat out p;
    fprintf out " -> ";
    print_expr out e
  | RecordCon [] -> fprintf out "{}"
  | RecordCon ((s0, e0) :: fs) -> 
    fprintf out "{%s=" s0; print_expr out e0; 
    List.iter (fun (s, e) -> fprintf out ", %s=" s; print_expr out e) fs;
    fprintf out "}"
  | IntLit i -> fprintf out "%d" i
  | BoolLit true -> fprintf out "true"
  | BoolLit false -> fprintf out "false"
  | Id s -> fprintf out "Id %s" s
  end;
  fprintf out " : "; print_ty out t; fprintf out ")"

and print_pat out (_p, _, _t) = 
  fprintf out "(";
  begin match _p with
  | Param s -> fprintf out "Param %s" s
  | RecPat [] -> fprintf out "[]"
  | RecPat ((s0, p0) :: fs) -> 
    fprintf out "{%s=" s0; print_pat out p0; 
    List.iter (fun (s, p) -> fprintf out ", %s=" s; print_pat out p) fs;
    fprintf out "}"
  | CatPat (p1, p2) -> print_pat out p1; fprintf out " | "; print_pat out p2
  end; 
  fprintf out " : ";
  print_ty out _t;
  fprintf out ")"

let parse ch = 
  let lexbuf = Lexing.from_channel ch in
  Parse.program_file Lex.token lexbuf

let check fname = 
  let ast = parse (File.open_in fname) in
  J.infer_defs Cyclic.empty ast

let print_ctx ctx = 
  Cyclic.to_list ctx |> List.iter @@ fun (name, (ty, _)) -> 
    printf "%s : " name;
    Types.Show.print_ty stdout ty;
    printf "\n"

let print_term_ctx ctx = 
  Types.Dict.to_list ctx |> List.iter @@ fun (name, lazy v) -> 
    printf "%s : " name;
    Eval.print_val v;
    printf "\n"

let interpret = flags "interpret sources" "interpret" 'i'

let () = match P.parse_argv op with
  | [] | _ :: _ :: _ -> P.usage op ()
  | [fname] -> 
    let ast = parse (File.open_in fname) in
    let ctx = J.infer_defs Cyclic.empty ast in
    (* print_prog stdout ast; *)
    print_ctx ctx;
  try
    if O.get interpret then
      (match Types.Dict.find_opt "main" (Eval.eval Types.Dict.empty ast) with
      | Some lazy v -> print_newline (); Eval.print_val v; print_newline ()
      | None -> failwith "no main function!")
    else exit 0
  with Eval.EvalErr (ctx, err, sp) -> 
      print_endline "\nStuck program.";
      Common.print_span stdout sp;
      Eval.print_err err;
      print_newline ();
      print_term_ctx ctx; 
      print_newline ();
      print_prog stdout ((parse (File.open_in fname))); print_newline ();
      exit 0
