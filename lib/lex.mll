{
  open! Batteries
  open! Lexing

  open Parse
}

let whitespace = ' '+ | '\t'
let eol = ['\r' '\n'] | '\r' '\n'
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '_' '0'-'9']*
let lit = ['0'-'9']+

rule token = parse
  | eof {EOF}
  | "$" {EOF}
  | whitespace {token lexbuf}
  | eol {new_line lexbuf; token lexbuf}

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

  | "true" {TRUE}
  | "false" {FALSE}
  | id as x {ID x}
  | lit as x {LIT (int_of_string x)}

  | _ as s {failwith (Printf.sprintf "Unexpected character %c" s)}
