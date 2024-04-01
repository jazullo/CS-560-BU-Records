open! Batteries
open Uref

module T2 = Tuple2
module T3 = Tuple3

open Ast

open Types.S
open Types.Unify
open Types

let fresh () = uref (MVar (!Common.level, unique ()))

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

let inter_t l r = Free.(add_t l (add_t r (mul_t l r)))

let rec infer ctx (_e, _sp, _t) = match _e with
  | Ternary (e1, e2, e3) -> 
    infer ctx e1; infer ctx e2; infer ctx e3;
    u "Ternary condition expects a bool" (_2 e1) (_3 e1) (uref (MLit MBool));
    u "Ternary branches expect the same type" (_2 e1) (_3 e2) (_3 e3);
    u "Unexpected result from ternary" (_2 e1) (_3 e2) _t
  | Apply (e, es) -> apply_many ctx _t e es
  | Arithmetic (e1, _, e2) -> 
    infer ctx e1; infer ctx e2;
    u "Arithmetic op expects int left arg" (_2 e1) (_3 e1) (uref (MLit MInt));
    u "Arithmetic op expects int right arg" (_2 e2) (_3 e2) (uref (MLit MInt));
    u "Arithmetic op expects int result" _sp _t (uref (MLit MInt))
  | Comparative (e1, _, e2) -> 
    infer ctx e1; infer ctx e2;
    u "Comparison op expects int left arg" (_2 e1) (_3 e1) (uref (MLit MInt));
    u "Comparison op expects int right arg" (_2 e2) (_3 e2) (uref (MLit MInt));
    u "Comparison op expects bool result" _sp _t (uref (MLit MBool))
  | Logical (e1, _, e2) -> 
    infer ctx e1; infer ctx e2;
    u "Logic op expects bool left arg" (_2 e1) (_3 e1) (uref (MLit MBool));
    u "Logic op expects bool right arg" (_2 e2) (_3 e2) (uref (MLit MBool));
    u "Logic op expects bool result" _sp _t (uref (MLit MBool))
  | Not e ->
    infer ctx e;
    u "Logic op expects bool arg" (_2 e) (_3 e) (uref (MLit MBool));
    u "Logic op expects bool result" _sp _t (uref (MLit MBool))
  | Record (e1, Intersect, e2) -> 
    infer ctx e1; infer ctx e2;
    let l = Free.fresh () in
    let r = Free.fresh () in
    u "Record op expects rec left arg" (_2 e1) (_3 e1) (uref (TRec l));
    u "Record op expects rec right arg" (_2 e2) (_3 e2) (uref (TRec r));
    u "Union of records is not compatable with expected result" _sp _t
      (uref (TRec (inter_t l r)))
  | Record (e1, Concatenate, e2) -> 
    infer ctx e1; infer ctx e2;
    let l = Free.fresh () in
    let r = Free.fresh () in
    u "Record op expects rec left arg" (_2 e1) (_3 e1) (uref (TRec l));
    u "Record op expects rec right arg" (_2 e2) (_3 e2) (uref (TRec r));
    u "Record op expects rec result" _sp _t
      (uref (TRec (Free.mul_t l r)))
  | Project (e, s) -> 
    infer ctx e;
    let a = Free.fresh () in  (* rest of the record *)
    let v = fresh () in  (* associated value *)
    u "Projection expects a record with the required field" (_2 e) (_3 e) @@
      uref (TRec (Free.mul_t a (Free.uconst (Fin, Dict.singleton s v))));
    u "Unexpected result type from projection" _sp _t v
  
  | Binding (s, ps, e1, a, e2) -> 
    incr Common.level; 
    abstract_many ctx a e1 ps; decr Common.level; 
    let ctx' = Cyclic.insert s (a, Poly (bound (_3 e1))) ctx in
    infer ctx' e2; 
    u "Unexpected type from let expression" _sp _t (_3 e2)

  | Abstract (ps, e) -> abstract_many ctx _t e ps
  
  | RecordCon rs -> 
    u "Unexpected record type" _sp _t
    (uref (TRec (Free.uconst (Fin, List.fold_left (fun acc (s, e) -> 
      infer ctx e;
      Dict.add s (_3 e) acc
    ) Dict.empty rs))))
  | IntLit _ -> u "Unexpected int type" _sp _t (uref (MLit MInt))
  | BoolLit _ -> u "Unexpected bool type" _sp _t (uref (MLit MBool))
  | Id s -> (match Cyclic.find_rec_opt s ctx with
    | None -> err _sp "Unbound Identifier" ("Cannot find ["^s^"].")
    | Some ((t, Mono), _) -> u "Identifier with unexpected type" _sp _t t
    | Some ((t, Poly w), _) -> 
      u "Identifier with unexpected type" _sp _t (generalize w t))

and apply_many ctx t0 e1 es = 
  infer ctx e1; List.iter (infer ctx) es;
  let t_result = fresh () in
  let t_args = List.fold_right (fun x acc -> uref (MFun (_3 x, acc))) es t_result in
  u "Unexpected argument type" (_2 e1) t_args (_3 e1);
  u "Unexpected result type" (_2 e1) t0 t_result

and abstract_many ctx t0 e1 ps = 
  let ctx', t_args = pat_many ctx ps in
  infer ctx' e1;
  u "Unexpected function type" (_2 e1) t0 @@
    List.fold_right (fun x acc -> uref (MFun (x, acc))) t_args (_3 e1)

and process_pat ctx (_p, _sp, v) = match _p with
  | Param s -> 
    Cyclic.insert s (v, Mono) ctx, v
  | RecPat xs -> 
    let ctx', consts = List.fold_left (fun (c, m) (s, p) -> 
        let c', t = process_pat c p in
        c', Dict.add s t m
      ) (ctx, Dict.empty) xs in
    uset v (TRec Free.(uconst (Fin, consts)));
    ctx', v
  | CatPat (p1, p2) -> 
    let ctx', t1 = process_pat ctx p1 in
    let ctx'', t2 = process_pat ctx' p2 in
    let rho1, rho2 = Free.(fresh (), fresh ()) in
    u "Cat pattern expects rec left pat" _sp t1 (uref (TRec rho1));
    u "Cat pattern expects rec right pat" _sp t2 (uref (TRec rho2));
    uset v (TRec (Free.mul_t rho1 rho2));
    ctx'', v

and pat_many ctx ps = 
  List.fold_left
    (fun (c, ts) p -> let c', t = process_pat c p in c', t :: ts)
    (ctx, []) ps
  |> T2.map2 List.rev

let infer_defs ctx = List.fold_left (fun ctx' ((name, args, body), _, a) -> 
  let ctx'' = Cyclic.insert name (a, Mono) ctx' in
  abstract_many ctx'' a body args;
  Cyclic.insert name (a, Poly Universe.empty) ctx''
) ctx
