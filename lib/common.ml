open! Batteries
open Lexing
open Printf

exception UnifError of string

let lincol_of_pos p = p.pos_lnum, p.pos_cnum - p.pos_bol + 1
let span_of_loc (p1, p2) = lincol_of_pos p1, lincol_of_pos p2

let print_span out loc = 
  let (l1, c1), (l2, c2) = span_of_loc loc in
  if l1 = l2 then fprintf out "Line %d, " l1
  else fprintf out "Lines %d-%d, " l1 l2;
  fprintf out "Columns %d-%d\n" c1 c2
