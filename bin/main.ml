(* open! Batteries
open Brml
open Ubool

module BoolC : (Constant) = struct
  type t = bool
  let and_const a b = a && b
  let xor_const a b = a <> b
  let one = true
  let zero = false
  let to_string a = if a then "⊤" else "⊥"
end

module B = Make(BoolC)
open B

let anf_expr = BExpr [
    [ref (BConst BoolC.one)];
    [ref (BVar 0); ref (BVar 1)];
    [ref (BVar 1); ref (BVar 2); ref (BVar 3); ref (BVar 4)];
    (* [ref (BVar 5)]; *)
  ]

let () = print_endline (pretty_print pretty_boolean_ anf_expr)

let () = print_endline (pretty_print pretty_prop (PXor [
    PConst BoolC.one;
    PAnd [PVar 0; PVar 1];
    PAnd [PVar 1; PVar 2; PVar 3; PVar 4];
    (* PAnd [PVar 5]; *)
  ]))

let () = print_endline (pretty_print pretty_prop (anf_to_prop anf_expr))
let () = print_endline (pretty_print pretty_prop (anf_no_xor (anf_to_prop anf_expr)))
let () = print_endline (pretty_print pretty_prop (anf_no_not (anf_no_xor (anf_to_prop anf_expr))))
let () = print_endline (pretty_print pretty_prop (anf_no_const (anf_no_not (anf_no_xor (anf_to_prop anf_expr)))))
let () = print_endline (pretty_print pretty_prop (anf_flatten (anf_no_const (anf_no_not (anf_no_xor (anf_to_prop anf_expr))))))
let () = print_endline (pretty_print pretty_prop (anf_distribute (anf_flatten (anf_no_const (anf_no_not (anf_no_xor (anf_to_prop anf_expr)))))))

let () = print_endline ""
let () = print_endline (pretty_print pretty_prop (PAnd [
    POr [PVar 0; PVar 1];
    POr [PVar 0; PNot (PVar 1)];
    POr [PNot (PVar 0); PVar 1];
    POr [PNot (PVar 0); PNot (PVar 1)];
  ]))
let () = print_endline (pretty_print pretty_prop (anf_distribute (PAnd [
    POr [PVar 0; PVar 1];
    POr [PVar 0; PNot (PVar 1)];
    POr [PNot (PVar 0); PVar 1];
    POr [PNot (PVar 0); PNot (PVar 1)];
  ]))) *)
