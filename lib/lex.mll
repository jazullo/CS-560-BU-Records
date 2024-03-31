{
  open! Batteries
  open! Lexing

  open Parse

  let last = ref false
  let que () = last := true
  let deq () = if !last then (last := false; decr Common.level)
}

let whitespace = ' '+ | '\t'
let eol = ['\r' '\n'] | '\r' '\n'
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '_' '0'-'9']*
let lit = ['0'-'9']+

rule token = parse
  | eof {deq (); EOF}
  | whitespace {deq (); token lexbuf}
  | eol {deq (); new_line lexbuf; token lexbuf}

  | "(" {deq (); LPAREN}
  | ")" {RPAREN}
  | "{" {deq (); LBRACE}
  | "}" {RBRACE}
  | "," {COMMA}
  | "." {PERIOD}
  | "fun" {FUN}
  | "->" {ARROW}

  | "+" {ADD}
  | "-" {SUB}
  | "*" {MUL}
  | "/" {DIV}
  | "%" {MOD}
  | "!" {NOT}
  | "||" {OR}
  | "&&" {AND}
  | "|" {CONCAT}
  | "&" {INTERSECT}
  | "=" {EQ}
  | "<>" {NE}
  | "<" {LT}
  | "<=" {LE}
  | ">" {GT}
  | ">=" {GE}

  | "def" {DEF}
  | "let" {incr Common.level; LET}
  | "in" {que (); IN}
  | "if" {IF}
  | "then" {THEN}
  | "else" {ELSE}

  | "true" {TRUE}
  | "false" {FALSE}
  | id as x {ID x}
  | lit as x {LIT (int_of_string x)}

  | _ as s {failwith (Printf.sprintf "Unexpected character %c" s)}
