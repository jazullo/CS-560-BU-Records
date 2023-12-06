open! Batteries
open Uref

module T2 = Tuple2
module T3 = Tuple3

open Ast

open Types.S
open Types.Unify
open Types

let fresh () = uref (MVar (unique ()))

let err _sp msg unif_msg = 
  print_endline "Type Error.";
  Printf.printf "";  (* print span *)
  print_endline (msg ^ ".");
  print_endline unif_msg

let u msg sp t1 t2 = (* unify w/ metadata *)
  try t1 =? t2 with
  Common.UnifError umsg -> err sp msg umsg

let _1 = T3.first
let _2 = T3.second
let _3 = T3.third

let rec infer ctx (_e, _sp, _t) = match _e with
  | Ternary (e1, e2, e3) -> 
    infer ctx e1; infer ctx e2; infer ctx e3;
    u "Ternary condition expects a bool" (_2 e1) (_3 e1) (uref (MLit MBool));
    u "Ternary branches expect the same type" (_2 e1) (_3 e2) _t;
    u "Ternary branches expect the same type" (_2 e1) (_3 e3) _t
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
  | LogicalUnary (_, e) ->
    infer ctx e;
    u "Logic op expects bool arg" (_2 e) (_3 e) (uref (MLit MBool));
    u "Logic op expects bool result" _sp _t (uref (MLit MBool))
  | Record (e1, Concatenate, e2) -> 
    infer ctx e1; infer ctx e2;
    let l = Free.fresh () in
    let r = Free.fresh () in
    let o = Free.fresh () in
    u "Record op expects rec left arg" (_2 e1) (_3 e1) (uref (TRec l));
    u "Record op expects rec right arg" (_2 e2) (_3 e2) (uref (TRec r));
    u "Record op expects rec result" _sp _t (uref (TRec o));
    begin try Free.unify o Free.(add_t l (add_t r (mul_t l r))) with 
      Common.UnifError msg -> err _sp "Union of records is not compatable with expected result" msg
    end
  | Record (e1, Intersect, e2) -> 
    infer ctx e1; infer ctx e2;
    let l = Free.fresh () in
    let r = Free.fresh () in
    let o = Free.fresh () in
    u "Record op expects rec left arg" (_2 e1) (_3 e1) (uref (TRec l));
    u "Record op expects rec right arg" (_2 e2) (_3 e2) (uref (TRec r));
    u "Record op expects rec result" _sp _t (uref (TRec o));
    begin try Free.unify o Free.(mul_t l r) with Common.UnifError msg -> 
        err _sp "Intersection of records is not compatable with expected result" msg
    end
  | Project (e, s) -> 
    infer ctx e;
    let a = Free.fresh () in  (* type of the record *)
    u "Projection expects a record" (_2 e) (_3 e) (uref (TRec a));
    let b = Free.fresh () in  (* type of the rest of the record *)
    let c = fresh () in       (* type associated with the field *)
    begin try Free.unify a Free.(add_t b (uconst (Fin, Dict.singleton s c)))
      with Common.UnifError msg -> err _sp "Record must contain the projected field" msg
    end;
    u "Unexpected projection result" _sp _t c
  
  | Binding (s, ps, e1, e2) -> 
    let a = fresh () in
    abstract_many ctx a e1 ps;
    let ctx' = Cyclic.insert s a ctx in
    infer ctx' e2

  | Abstract (ps, e) -> abstract_many ctx _t e ps
  
  | RecordCon rs -> 
    u "Unexpected record type" _sp _t
    (uref (TRec (Free.uconst (Fin, List.fold_left (fun acc (s, e) -> 
      infer ctx e;
      Dict.add s (_3 e) acc
    ) Dict.empty rs))))
  | IntLit _ -> u "Unexpected int type" _sp _t (uref (MLit MInt))
  | BoolLit _ -> u "Unexpected bool type" _sp _t (uref (MLit MBool))

and apply_many ctx t0 e1 es = 
  infer ctx e1; List.iter (infer ctx) es;
  let t_result = fresh () in
  let t_args = List.fold_right (fun x acc -> uref (MFun (_3 x, acc))) es t_result in
  u "Unexpected argument type" (_2 e1) t_args (_3 e1);
  u "Unexpected result type" (_2 e1) t0 t_result

and abstract_many ctx t0 e1 ps = 
  let ctx' = List.fold_left (fun c s -> Cyclic.insert s (fresh ()) c) ctx ps in
  infer ctx' e1;
  let t_args = List.map (fun s -> Cyclic.find_rec s ctx' |> fst) ps in
  u "Unexpected function type" (_2 e1) t0 @@
    List.fold_right (fun x acc -> uref (MFun (x, acc))) t_args (_3 e1)

let infer_defs ctx defs = 
  let t_defs_full = List.map (fun (name, args, body) -> name, fresh (), args, body) defs in
  let t_defs = List.map (fun (name, v, _, _) -> name, v) t_defs_full in
  let ctx' = Cyclic.insert_many t_defs ctx in
  List.iter (fun (_, v, args, body) -> 
    abstract_many ctx' v body args;
  ) t_defs_full
