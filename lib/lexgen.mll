(* For types and generalization problems *)
{ open! Batteries open! Lexing open! Parsegen }

let whitespace = ' '+ | '\t'
let eol = ['\r' '\n'] | '\r' '\n'
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '_' '0'-'9']*
let lit = ['0'-'9']+

rule token = parse
  | eof {EOF}
  | '$' {EOF}
  | whitespace {token lexbuf}
  | eol {token lexbuf}
  | ("a" id) as x {AID x}
  | ("b" id) as x {BID x}
  
  | "(" {LPAREN}
  | ")" {RPAREN}
  | "{" {LBRACE}
  | "}" {RBRACE}
  | "->" {ARROW}
  | "+" {ADD}
  | "||" {OR}
  | "," {COMMA}
  | ";" {SEMICOLON}

  | id as x {ID x}
  | "int" {INT}
  | "bool" {BOOL}
  | ":" {COLON}

  | "<" {LANGLE}
  | ">" {RANGLE}
  | "!" {BANG}
