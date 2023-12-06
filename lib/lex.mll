{
  open! Batteries
  open! Lexing

  open Parse
}

let whitespace = ' '+ | ['\r' '\n'] | '\r' '\n' | '\t'
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '_' '0'-'9']*
let lit = ['0'-'9']+

rule token = parse
  | "eof" {EOF}
  | "$" {EOF}
  | whitespace {token lexbuf}

  | "(" {LPAREN}
  | ")" {RPAREN}
  | "{" {LBRACE}
  | "}" {RBRACE}
  | "," {COMMA}
  | "." {PERIOD}
  | "\\" {BACKSLASH}
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
  | "let" {LET}
  | "in" {IN}
  | "if" {IF}
  | "then" {THEN}
  | "else" {ELSE}
  | "end" {END}

  | id as x {ID x}
  | lit as x {LIT (int_of_string x)}
  | "true" {TRUE}
  | "false" {FALSE}

  | _ as s {failwith (Printf.sprintf "Unexpected character %c" s)}
