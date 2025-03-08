open! Batteries
open! Brml

let gen str = 
  let lexbuf = Lexing.from_string (str^"$") in
  let rows, vars = Parsegen.system Lexgen.token lexbuf in
  let arr = Array.of_list rows in
  Types.BGen.gen vars [] arr;
  let p = 
    Array.print ~first:"" ~last:"" ~sep:", "
      (Types.Show.print_rec_ty ~delim:false) in
  let s = IO.output_string () in
  p s arr;
  IO.close_out s

let g = gen %> print_endline

let%expect_test _ = g "b0"; [%expect"b0"]
let%expect_test _ = g "{x : bool}"; [%expect"{x : bool}"]
(* let%expect_test _ = g "{x : bool} b1"; [%expect"{x : bool} b1"] *)
