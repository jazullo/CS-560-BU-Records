open! Batteries

type span = Lexing.position * Lexing.position

type arith_op = Add | Sub | Mul | Div | Mod
type cmp_op = Eq | Ne | Gt | Lt | Ge | Le
type logic_op = And | Or
type rec_op = Concatenate | Intersect | Update

type expr = _expr * span * Types.Tau.t
and _expr = 
  | Ternary of expr * expr * expr
  | Apply of expr * expr
  | Arithmetic of expr * arith_op * expr
  | Comparative of expr * cmp_op * expr
  | Logical of expr * logic_op * expr
  | Not of expr
  | Record of expr * rec_op * expr
  | Project of expr * string
  | Binding of string * expr * expr
  | Abstract of pat * expr
  | RecordCon of (string * expr) list
  | IntLit of int
  | BoolLit of bool
  | Id of string

and pat = _pat * span * Types.Tau.t
and _pat = 
  | Param of string
  | RecPat of (string * pat) list
  | CatPat of pat * pat

type def = _def * span * Types.Tau.t
and _def = string * expr
