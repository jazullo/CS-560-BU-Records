open! Batteries
open! Brml

let pp = Array.print ~first:"" ~last:"" ~sep:", " (Types.Show.print_rec_ty ~delim:false)
let pp_string arr = 
  let s = IO.output_string () in
  pp s arr;
  IO.close_out s

let gen str = 
  let lexbuf = Lexing.from_string (str^"$") in
  let rows, vars = Parsegen.system Lexgen.token lexbuf in
  let arr = Array.of_list rows in
  Gbool.gen vars [] arr;
  pp_string arr

let g = gen %> print_endline

(* sanity checks *)
let%expect_test _ = g "b0"; [%expect"b0"]
let%expect_test _ = g "{x : bool}"; [%expect"{x : bool}"]
let%expect_test _ = g "{x : bool} b0"; [%expect"{x : bool} b0"]

(* lone polynomial tests *)
let%expect_test _ = g "b0 b1"; [%expect"b0"]
let%expect_test _ = g "b0 + b1"; [%expect"b1"]

let%expect_test _ = g "{x : bool} b0 b1"; [%expect"{x : bool} b0"]
let%expect_test _ = g "{x : bool} (b0 + b1)"; [%expect"{x : bool} b1"]

let%expect_test _ = g "b0 + {x : bool} b1"; [%expect"b1"]
let%expect_test _ = g "{x : bool} b0 + !{x : bool} b1"; [%expect"b1"]
let%expect_test _ = g "{x : bool} b0 + !{y : bool} b1"; [%expect"!{y : bool} b1"]

(* multiterm tests *)
let%expect_test _ = g "b0, b0"; [%expect"b0, b0"]
let%expect_test _ = g "{x : bool} b0, b0"; [%expect"{x : bool} b0, b0"]
let%expect_test _ = g "b0, {x : bool} b0"; [%expect"b0, {x : bool} b0"]
let%expect_test _ = g "{x : bool, y : bool} b0, {x : bool} b0";
  [%expect"{x : bool, y : bool} b0, {x : bool} b0"]
let%expect_test _ = g "b0 + b1, b0 + b1"; [%expect"b1, b1"]
let%expect_test _ = g "b0 b1, b0 b1"; [%expect"b0, b0"]

let%expect_test _ = g "{x : bool} (b0 + b1), {x : bool} (b0 + b1)";
  [%expect"{x : bool} b1, {x : bool} b1"]

(* multiple variables involving factorization of coefficients *)
let%expect_test _ = g "{x : bool} (b0 + b1), {x : bool} (b0 + b1)";
  [%expect"{x : bool} b1, {x : bool} b1"]
(* let%expect_test _ = g "{x : bool} (b0 + b1), {y : bool} (b0 + b1)";
  [%expect"{x : bool} b1, {y : bool} b1"] *)
(* let%expect_test _ = g "{x : bool} (b0 + b1), {x : bool, y : bool} (b0 + b1)";
  [%expect"{x : bool} b1, {x : bool, y : bool} b1"] *)
(* let%expect_test _ = g "b0 + b1, b0 b1";
  [%expect"b0 + b1, b0 b1"] *)



(* factorization of polynomials *)
(* let%expect_test _ = g "(b0 + b1 + !{}) b2 + (b0 + b1 + !{}) (b3 + b4 + !{}) b5"; [%expect""] *)
(* add another product with (b0 + b1 + !{}) to test factorization *)
