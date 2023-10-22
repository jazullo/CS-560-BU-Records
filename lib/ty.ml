open! Batteries

type t = 
  | TVar of string
  | TLit of lit
  | TFun of t * t
  | TRec of recty

and lit = 
  | TInt
  | TBool

and recty = 
  | RAnd of recty * recty
  | ROr of recty * recty
  | RNot of recty
  | RCon of (string * t) list
