open! Batteries
open! Ast

module Dict = Map.Make(String)

type value =
  | FunVal of string list * expr
  | RecordVal of value Dict.t
  | IntVal of int
  | BoolVal of bool

let rec interpret_expr e ctx =
  let (_e, _, _) = e in
  match _e with
  | Ternary (cond, e1, e2) ->
    let a = match interpret_expr cond ctx with BoolVal b -> b | _ -> assert false in
    interpret_expr (if a then e1 else e2) ctx
  | Apply (e, params) ->
    let a, b = match interpret_expr e ctx with FunVal (p, e) -> p, e | _ -> assert false in
    let c = List.map (fun param -> interpret_expr param ctx) params in
    let new_ctx = Dict.merge (fun _ x y -> match x, y with
      | Some _, Some y -> Some y
      | None, y -> y
      | x, None -> x
    ) ctx (Dict.of_list (List.combine a c)) in
    interpret_expr b new_ctx
  | Arithmetic (e1, op, e2) ->
    let a = match interpret_expr e1 ctx with IntVal i -> i | _ -> assert false in
    let b = match interpret_expr e2 ctx with IntVal i -> i | _ -> assert false in
    begin match op with
      | Add -> IntVal (a + b)
      | Sub -> IntVal (a - b)
      | Mul -> IntVal (a * b)
      | Div -> IntVal (a / b)
      | Mod -> IntVal (a mod b)
    end
  | Comparative (e1, op, e2) ->
    let a = match interpret_expr e1 ctx with IntVal i -> i | _ -> assert false in
    let b = match interpret_expr e2 ctx with IntVal i -> i | _ -> assert false in
    begin match op with
      | Eq -> BoolVal (a = b)
      | Ne -> BoolVal (a <> b)
      | Lt -> BoolVal (a < b)
      | Le -> BoolVal (a <= b)
      | Gt -> BoolVal (a > b)
      | Ge -> BoolVal (a >= b)
    end
  | Logical (e1, op, e2) ->
    let a = match interpret_expr e1 ctx with BoolVal b -> b | _ -> assert false in
    let b = match interpret_expr e2 ctx with BoolVal b -> b | _ -> assert false in
    begin match op with
      | And -> BoolVal (a && b)
      | Or -> BoolVal (a || b)
      | Not -> assert false
    end
  | LogicalUnary (op, e) ->
    let a = match interpret_expr e ctx with BoolVal b -> b | _ -> assert false in
    begin match op with
      | Not -> BoolVal (not a)
      | _ -> assert false
    end
  | Project (e, s) ->
    let a = match interpret_expr e ctx with RecordVal r -> r | _ -> assert false in
    a |> Dict.find s
  | RecordCon l -> RecordVal (Dict.of_list (List.map (fun (s, e) -> (s, interpret_expr e ctx)) l))
  | IntLit i -> IntVal i
  | BoolLit b -> BoolVal b
  | Ref s -> ctx |> Dict.find s
  | _ -> assert false

let interpret_def def ctx =
  let (_def, _) = def in
  let (_, _, e) = _def in
  interpret_expr e ctx

let interpret_defs defs =
  let ctx = Dict.of_list (List.map (fun def -> let (_def, _) = def in let (name, params, e) = _def in (name, FunVal (params, e))) defs) in
  let is_main = fun def -> let (_def, _) = def in let (name, _, _) = _def in name = "main" in
  interpret_def (List.find is_main defs) ctx
