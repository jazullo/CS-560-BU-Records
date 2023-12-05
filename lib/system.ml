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
  | Apply (e, es) ->  (* maybe simplify to not use a list *)
    infer ctx e;
    let rec go t0 e1 = function
      | [] -> ()
      | h :: t -> 
        let v = fresh () in
        go v (T3.map3 (fun _ -> v) e1) t;
        apply ctx t0 e1 h in
    go _t e es
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
    u "Logic op expects int right arg" (_2 e2) (_3 e2) (uref (MLit MBool));
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
  
  | Binding _ -> failwith "todo"    (* these will be annoying *)
  | Abstract _ -> failwith "todo"
  
  | RecordCon rs -> 
    u "Unexpected record type" _sp _t
    (uref (TRec (Free.uconst (Fin, List.fold_left (fun acc (s, e) -> 
      infer ctx e;
      Dict.add s (_3 e) acc
    ) Dict.empty rs))))
  | IntLit _ -> u "Unexpected int type" _sp _t (uref (MLit MInt))
  | BoolLit _ -> u "Unexpected bool type" _sp _t (uref (MLit MBool))

and apply ctx t0 e1 e2 = 
  infer ctx e2;
  let t3 = fresh () in
  let t4 = fresh () in
  u "Applicands must be functions" (_2 e1) (_3 e1) (uref (MFun (t3, t4)));
  u "Unexpected argument type" (_2 e2) t3 (_3 e2);
  u "Unexpected result type" (_2 e1) t4 t0

    
