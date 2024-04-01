open! Batteries
open! Uref

open Ast
open Types
open! J

let rec deepcopy (_e, _sp, _t) = (match _e with
  | Ternary (e1, e2, e3) -> Ternary (deepcopy e1, deepcopy e2, deepcopy e3)
  | Apply (e, es) -> Apply (deepcopy e, List.map deepcopy es)
  | Arithmetic (e1, op, e2) -> Arithmetic (deepcopy e1, op, deepcopy e2)
  | Comparative (e1, op, e2) -> Comparative (deepcopy e1, op , deepcopy e2)
  | Logical (e1, op, e2) -> Logical (deepcopy e1, op, deepcopy e2)
  | Not e -> Not (deepcopy e)
  | Record (e1, op, e2) -> Record (deepcopy e1, op, deepcopy e2)
  | Project (e, s) -> Project (deepcopy e, s)
  | Binding (s, ps, e1, t, e2) -> Binding (s, ps, deepcopy e1, Unify.deepcopy t, deepcopy e2)
  | Abstract (ps, e) -> Abstract (ps, deepcopy e)
  | RecordCon r -> RecordCon (List.map (Tuple2.map2 deepcopy) r)
  | IntLit _ | BoolLit _ | Id _ -> _e
), _sp, Unify.deepcopy _t

type value = 
  | VInt of int
  | VBool of bool
  | VClosure of (string, value) Cyclic.t * pat list * expr * S.t
  | VRec of value Dict.t

let rec print_val = let open Printf in function
  | VInt i -> printf "%d" i
  | VBool b -> print_bool b
  | VRec r -> 
    printf "%s" "{";
    (match Dict.to_list r with
    | [] -> ()
    | (s, v) :: t -> printf "%s = " s; print_val v; 
      List.iter (fun (s', v') -> printf ", %s = " s'; print_val v') t)
  | VClosure _ -> printf "%s" "<fun>"

let rec value_type = function
  | VInt _ -> uref (S.MLit MInt)
  | VBool _ -> uref (S.MLit MBool)
  | VClosure (_, _, _, t) -> t
  | VRec vs -> uref (S.TRec (Dict.fold (fun s w -> 
      Free.mul_t (Free.uconst (Fin, Dict.singleton s (value_type w)))
    ) vs (Free.uconst (Fin, Dict.empty))))

let deepcopy_val = function       (* copy context? *)
  | VClosure (c, ps, e, t) -> VClosure (c, ps, deepcopy e, Unify.deepcopy t)
  | v -> v

let concretize_rec rho = match uget rho with
  | Free.Var _ -> Free.unify rho (Free.uconst (Inv, Dict.empty))
  | Free.Expr e -> List.iter (snd %> List.iter (Free.unify (Free.uconst (Inv, Dict.empty)))) e

let get_type_args t = 
    let rec go x = match uget x with
      | S.MFun (i, o) -> i :: go o
      | _ -> [] in
    go t

let conditional_monomorph e t_args es = 
  if List.for_all (fun (t, (_, _, t')) -> t = t') (List.combine t_args es)
  then e
  else
    let e_ = deepcopy e in
    let t_args = get_type_args (_3 e_) in
    List.iter (fun ((_, _, t'), t) -> Unify.(t =? t')) (List.combine es t_args);
    e_

let rec take_type n t = match uget t with
  | S.MFun (_, o) -> 
    if n > 0 then take_type (n-1) o
    else o
  | _ -> failwith "failure"

let rec (==>) (ctx : (string, value) Cyclic.t) (_e, _sp, _t) = match _e with
  | Ternary (e1, e2, e3) -> 
    begin match ctx ==> e1 with
      | VBool true -> ctx ==> e2
      | VBool false -> ctx ==> e3
      | VInt _ -> failwith "Type error: int in guard"
      | VClosure _ -> failwith "Type error: fun in guard"
      | VRec _ -> failwith "Type error: rec in guard"
    end
  (* | Apply (e, []) -> ctx ==> e *)
  | Apply (e, es) -> 
    let (ctx', ps, e0, t) = match ctx ==> e with
      | VClosure (c, ps, e, t) -> c, ps, e, t
      | _ -> failwith "Application to non-lambda" in
    apply ctx' ps e0 es t
  | Arithmetic (e1, op, e2) -> begin match ctx ==> e1, ctx ==> e2 with
    | VInt i, VInt j -> VInt ((match op with
      | Add -> (+)
      | Sub -> (-)
      | Mul -> ( * )
      | Div -> ( / )
      | Mod -> (fun x y -> (if x > y then Fun.id else Int.neg) (x mod y))) i j)
    | _ -> failwith "adding non integers" end
  | Comparative (e1, op, e2) -> begin match ctx ==> e1, ctx ==> e2 with
    | VInt i, VInt j -> VBool ((match op with
      | Eq -> (=)
      | Ne -> (<>)
      | Gt -> (>)
      | Lt -> (<)
      | Ge -> (>=)
      | Le -> (<=)) i j)
    | _ -> failwith "comparing non integers" end
  | Logical (e1, op, e2) -> (match op with
    | And -> (match ctx ==> e1 with
      | VBool true -> ctx ==> e2
      | VBool false -> VBool false
      | _-> failwith "and left operand nonboolean"
      )
    | Or -> (match ctx ==> e1 with
      | VBool true -> VBool true
      | VBool false -> ctx ==> e2
      | _ -> failwith "or left operand nonboolean")
  )
  | Not e -> (match ctx ==> e with
    | VBool b -> VBool (not b)
    | _ -> failwith "Negating nonboolean")
  | Record (e1, op, e2) -> (match ctx ==> e1, ctx ==> e2 with
    | VRec r1, VRec r2 -> (match op with
      | Concatenate -> VRec (Dict.union (fun _ _ x -> Some x) r1 r2)
      | Intersect -> VRec (Dict.merge (fun _ -> function
        | Some _ -> Fun.id
        | None -> Fun.const None) r1 r2))
    | _ -> failwith "Record operation over nonproduct")
  | Project (e, s) -> (match ctx ==> e with
    | VRec r -> (match Dict.find_opt s r with
      | Some v -> v
      | None -> failwith ("Cannot project nonpresent field " ^ s))
    | _ -> failwith "Cannot project from nonrecord")
  
  | Binding (s, [], e1, _, e2) -> Cyclic.insert s (ctx ==> e1) ctx ==> e2
  | Binding (s, ps, e1, t, e2) -> Cyclic.insert s (VClosure (ctx, ps, e1, t)) ctx ==> e2
  | Abstract (ps, e) -> VClosure (ctx, ps, e, _t)
  | RecordCon asgns -> VRec (Dict.of_list (List.map (T2.map2 ((==>) ctx)) asgns))
  | IntLit i -> VInt i
  | BoolLit b -> VBool b
  | Id s -> fst (Cyclic.find_rec s ctx)

and apply ctx ps e0 es t = 
  let t_args = get_type_args t in
  let les = List.length es in
  let pes = List.length ps in
  match[@warning "-8"] compare pes les with
  | 0 -> 
    let t_args_trunc = List.take les t_args in
    assert (List.length t_args >= les);
    let e1 = conditional_monomorph e0 t_args_trunc es in
    let ctx' = eval_cases ctx ps es in
    ctx' ==> e1
  | 1 -> 
    let t_args_trunc = List.take les t_args in
    let e1 = conditional_monomorph e0 t_args_trunc es in
    let lps, rps = List.takedrop (pes - les) ps in
    let ctx' = eval_cases ctx rps es in
    VClosure (ctx', lps, e1, take_type (pes - les) (_3 e1))
  | -1 -> 
    let es_l, es_r = List.takedrop pes es in
    let t_args_trunc = List.take pes t_args in
    let e1 = conditional_monomorph e0 t_args_trunc es_l in
    let ctx1 = eval_cases ctx ps es_l in
    match (ctx1 ==> e1) with
    | VClosure (ctx2, ps', e', t') -> apply ctx2 ps' e' es_r t'
    | _ -> failwith "Application to non-lambda 2"

and case ctx (p, _, _) = match p with
  | Param s -> fun v -> Cyclic.insert s v ctx
  | RecPat asgns -> begin function
      | VRec d -> List.fold_left (fun c (s, p') -> Dict.find_opt s d |> function
        | Some x -> case c p' x
        | None -> failwith ("Record is missing field " ^ s)) ctx asgns
      | _ -> failwith "Matching nonrecord against record pattern"
    end
  | CatPat ((_p1, _, _t1 as p1), (_p2, _, _t2 as p2)) -> begin match uget _t1, uget _t2 with
      | S.TRec rho1, S.TRec rho2 -> 
        concretize_rec rho1; concretize_rec rho2;
        begin match Free.simplify (uget rho1), Free.simplify (uget rho2) with
          | Var _, _ | _, Var _ -> failwith "poly record at runtime"
          | Expr [_, _ :: _], Expr [_, _] | Expr [_, _], Expr [_, _ :: _] -> 
            failwith "partially poly record at runtime"
          | Expr [(Inv, _), []], Expr [_, []] | Expr [_, []], Expr [(Inv, _), []] -> 
            failwith "unconstructable record at runtime"
          | Expr [(Fin, c1), []], Expr [(Fin, c2), []] -> begin function 
            | VRec d -> 
              let ctx' = case ctx p1 (VRec (Dict.filter (fun s _ -> Dict.mem s c1) d)) in
              case ctx' p2 (VRec (Dict.filter (fun s _ -> Dict.mem s c2) d))
            | _ -> failwith "Cat pattern matched against non-record"
          end
          | _ -> failwith "Bad record"
        end
      | _ -> failwith "Cat pattern with non-record subpatterns"
    end

and eval_cases ctx ps es = 
  List.fold_left (fun c (p, e) -> case c p (c ==> e)) ctx (List.combine ps es)

let eval ctx defs = 
  List.fold_left (fun c -> function
    | (s, (_ :: _ as ps), e), _, t -> Cyclic.insert s (VClosure (c, ps, e, t)) c
    | (s, [], e), _, _ -> Cyclic.insert s (c ==> e) c  (* no recursive values anyway *)
  ) ctx defs
