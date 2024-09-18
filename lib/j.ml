open! Batteries
open Uref

module T2 = Tuple2
module T3 = Tuple3

open Ast

open Types.Tau
open Types.Unify
open Types

let err _sp msg unif_msg = 
  print_endline "Type Error.";
  Common.print_span stdout _sp;
  print_endline (msg ^ ".");
  print_endline unif_msg;
  exit 1

let u msg sp t1 t2 = (* unify w/ metadata *)
  try t1 =? t2 with
  Common.UnifError umsg -> err sp msg umsg

let _1 = T3.first
let _2 = T3.second
let _3 = T3.third

let union_t l r = Tau.(add_t l (add_t r (mul_t l r)))

let rec infer ctx (_e, _sp, _t) = match _e with
  | Ternary (e1, e2, e3) -> 
    infer ctx e1; infer ctx e2; infer ctx e3;
    u "Ternary condition expects a bool" (_2 e1) (_3 e1) (const0 ABool);
    u "Ternary branches expect the same type" (_2 e1) (_3 e2) (_3 e3);
    u "Unexpected result from ternary" (_2 e1) (_3 e2) _t
  | Apply (e1, e2) -> apply ctx _t e1 e2
  | Arithmetic (e1, _, e2) -> 
    infer ctx e1; infer ctx e2;
    u "Arithmetic op expects int left arg" (_2 e1) (_3 e1) (const0 AInt);
    u "Arithmetic op expects int right arg" (_2 e2) (_3 e2) (const0 AInt);
    u "Arithmetic op expects int result" _sp _t (const0 AInt)
  | Comparative (e1, _, e2) -> 
    infer ctx e1; infer ctx e2;
    u "Comparison op expects int left arg" (_2 e1) (_3 e1) (const0 AInt);
    u "Comparison op expects int right arg" (_2 e2) (_3 e2) (const0 AInt);
    u "Comparison op expects bool result" _sp _t (const0 ABool)
  | Logical (e1, _, e2) -> 
    infer ctx e1; infer ctx e2;
    u "Logic op expects bool left arg" (_2 e1) (_3 e1) (const0 ABool);
    u "Logic op expects bool right arg" (_2 e2) (_3 e2) (const0 ABool);
    u "Logic op expects bool result" _sp _t (const0 ABool)
  | Not e ->
    infer ctx e;
    u "Logic op expects bool arg" (_2 e) (_3 e) (const0 ABool);
    u "Logic op expects bool result" _sp _t (const0 ABool)
  | Record (e1, Intersect, e2) -> 
    infer ctx e1; infer ctx e2;
    u "Record intersection is not compatable with expected result" _sp _t
      (mul_t (_3 e1) (_3 e2))
  | Record (e1, Concatenate, e2) -> 
    infer ctx e1; infer ctx e2;
    u "Union of records is not compatable with expected result" _sp _t
      (union_t (_3 e1) (_3 e2))
  | Project (e, s) -> 
    infer ctx e;
    let a = fresh () in  (* rest of the record *)
    let v = fresh () in  (* associated value *)
    u "Projection expects a record with the required field" (_2 e) (_3 e) @@
      union_t a (brec s v);
    u "Unexpected result type from projection" _sp _t v
  
  | Binding (s, e1, e2) -> 
    incr Common.level; 
    infer ctx e1; decr Common.level; 
    let ctx' = Cyclic.insert s (_3 e1, Poly (bound (_3 e1))) ctx in
    infer ctx' e2; 
    u "Unexpected type from let expression" _sp _t (_3 e2)

  | Abstract (p, e) -> abstract ctx _t e p
  
  | RecordCon rs -> 
    u "Unexpected record type" _sp _t
      (List.fold_left (fun a (x, e) -> infer ctx e; 
        add_t a (brec x (_3 e))) (uref (Expr [])) rs)
  | IntLit _ -> u "Unexpected int type" _sp _t (const0 AInt)
  | BoolLit _ -> u "Unexpected bool type" _sp _t (const0 ABool)
  | Id s -> (match Cyclic.find_rec_opt s ctx with
    | None -> err _sp "Unbound Identifier" ("Cannot find ["^s^"].")
    | Some ((t, Mono), _) -> u "Identifier with unexpected type" _sp _t t
    | Some ((t, Poly w), _) -> 
      u "Identifier with unexpected type" _sp _t (generalize w t))
  
and apply ctx t0 e1 e2 = 
  infer ctx e1; infer ctx e2;
  let t_result = fresh () in
  u "Unexpected argument type" (_2 e1) (bfun (_3 e2) t_result) (_3 e1);
  u "Unexpected function type" (_2 e1) t0 t_result

and abstract ctx t0 e1 p = 
  let ctx', t_arg = process_pat ctx p in
  infer ctx' e1;
  u "Unexpected function type" (_2 e1) t0 (bfun t_arg (_3 e1))

and process_pat ctx (_p, _sp, v) = match _p with
  | Param s -> Cyclic.insert s (v, Mono) ctx, v
  | RecPat xs -> 
    List.fold_left (fun (c, t) (x, p) -> 
      Tuple2.map2 (brec x %> add_t t) (process_pat c p)
    ) (ctx, uref (Expr [])) xs
  | CatPat (p1, p2) -> 
    let ctx', t1 = process_pat ctx p1 in
    let ctx'', t2 = process_pat ctx' p2 in
    u "Incompatible concatenation pattern" _sp v (union_t t1 t2);
    ctx'', v

let infer_defs ctx = List.fold_left (fun ctx' ((name, body), _, a) -> 
  let ctx'' = Cyclic.insert name (a, Mono) ctx' in
  infer ctx'' body;
  a =? _3 body;
  Cyclic.insert name (a, Poly Set.empty) ctx''
) ctx
