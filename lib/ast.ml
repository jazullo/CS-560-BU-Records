open! Batteries

type span = Lexing.position * Lexing.position

type arith_op = Add | Sub | Mul | Div | Mod
type cmp_op = Eq | Ne | Gt | Lt | Ge | Le
type logic_op = And | Or | Not
type rec_op = Concatenate | Intersect

type expr = _expr * span * Types.S.t
and _expr = 
  | Ternary of expr * expr * expr
  | Apply of expr * expr list
  | Arithmetic of expr * arith_op * expr
  | Comparative of expr * cmp_op * expr
  | Logical of expr * logic_op * expr
  | LogicalUnary of logic_op * expr
  | Record of expr * rec_op * expr
  | Project of expr * string
  | Binding of string * pat list * expr * expr
  | Abstract of pat list * expr
  | RecordCon of (string * expr) list
  | IntLit of int
  | BoolLit of bool
  | Id of string

and pat = _pat * span
and _pat = 
  | Param of string
  | IntPat of int
  | BoolPat of bool
  | RecPat of (string * pat) list
  | CatPat of pat * pat

type def = _def * span
and _def = string * pat list * expr
