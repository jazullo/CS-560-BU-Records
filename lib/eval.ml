open! Batteries
open! Uref

open Ast
open Types
open! J

let rec deepcopy (_e, _sp, _t) = (match _e with
  | Ternary (e1, e2, e3) -> Ternary (deepcopy e1, deepcopy e2, deepcopy e3)
  | Apply (e1, e2) -> Apply (deepcopy e1, deepcopy e2)
  | Arithmetic (e1, op, e2) -> Arithmetic (deepcopy e1, op, deepcopy e2)
  | Comparative (e1, op, e2) -> Comparative (deepcopy e1, op , deepcopy e2)
  | Logical (e1, op, e2) -> Logical (deepcopy e1, op, deepcopy e2)
  | Not e -> Not (deepcopy e)
  | Record (e1, op, e2) -> Record (deepcopy e1, op, deepcopy e2)
  | Project (e, s) -> Project (deepcopy e, s)
  | Binding (s, e1, e2) -> Binding (s, deepcopy e1, deepcopy e2)
  | Abstract (p, e) -> Abstract (p, deepcopy e)
  | RecordCon r -> RecordCon (List.map (Tuple2.map2 deepcopy) r)
  | IntLit _ | BoolLit _ | Id _ -> _e
), _sp, Unify.deepcopy _t

type value = 
  | VInt of int
  | VBool of bool
  | VClosure of value Lazy.t Dict.t * pat * expr * S.t
  | VRec of value Dict.t

let rec print_val = let open Printf in function
  | VInt i -> printf "%d" i
  | VBool b -> print_bool b
  | VRec r -> 
    printf "%s" "{";
    (match Dict.to_list r with
    | [] -> ()
    | (s, v) :: t -> printf "%s = " s; print_val v; 
      List.iter (fun (s', v') -> printf ", %s = " s'; print_val v') t);
    printf "%s" "}"
  | VClosure _ -> printf "%s" "<fun>"

let rec show_val = let open Printf in function
  | VInt i -> sprintf "%d" i
  | VBool b -> string_of_bool b
  | VRec r -> "{" ^ (match Dict.to_list r with
    | [] -> ""
    | (s, v) :: t -> sprintf "%s = " s ^ show_val v ^
      String.concat "" (List.map (fun (s', v') -> sprintf ", %s = " s' ^ show_val v') t))
  | VClosure _ -> "<fun>"

type eval_err_type = 
  | BadGuard of string
  | ApplicationNonArrow of string
  | ApplicationNonLambda
  | AddingNonIntegers | ComparingNonIntegers
  | AndLeftOpNonbool | OrLeftOpNonbool | NegateBool
  | RecOpNonproduct | ProjectAbsentField of string | ProjectNonproduct
  | MatchAbsentField of string | MatchNonrecordOnRecord | MatchNonrecordOnCat
  | MiscBadRecord | MatchCatNonsubproducts

let print_err = let open Printf in function
  | BadGuard s -> printf "Nonbool [%s] in guard.\n" s
  | ApplicationNonArrow s -> printf "Application to expr with nonarrow [%s].\n" s
  | ApplicationNonLambda -> print_endline "Application to nonlambda."
  | AddingNonIntegers -> print_endline "Addition with nonintegers."
  | ComparingNonIntegers -> print_endline "Comparison with nonintegers."
  | AndLeftOpNonbool -> print_endline "Left operand of [&&] is nonboolean."
  | OrLeftOpNonbool -> print_endline "Left operand of [||] is nonboolean."
  | NegateBool -> print_endline "Operand of [!] is nonbool."
  | RecOpNonproduct -> print_endline "Record operation between nonrecords."
  | ProjectAbsentField s -> printf "Cannot project field [%s] from record without it.\n" s
  | ProjectNonproduct -> print_endline "Cannot project field from nonrecord."
  | MatchAbsentField s -> printf 
    "Cannot match record pattern labeled [%s] against record without it.\n" s
  | MatchNonrecordOnRecord -> print_endline "Cannot match nonrecord against record pattern."
  | MatchNonrecordOnCat -> print_endline "Cannot match nonrecord against concat pattern."
  | MiscBadRecord -> print_endline "Bad record."
  | MatchCatNonsubproducts -> print_endline "Concat pattern must have record subpatterns."

exception EvalErr of value Lazy.t Dict.t * eval_err_type * span
let crash ctx err sp = raise (EvalErr (ctx, err, sp))

let deepcopy_val = function       (* copy context? *)
  | VClosure (c, ps, e, t) -> VClosure (c, ps, deepcopy e, Unify.deepcopy t)
  | v -> v

let concretize_rec rho = match uget rho with
  | Free.Var _ -> Free.unify rho (Free.uconst (Inv, Dict.empty))
  | Free.Expr e -> List.iter (snd %> List.iter (Free.unify (Free.uconst (Inv, Dict.empty)))) e

let rec (==>) (ctx : value Lazy.t Dict.t) (_e, _sp, _t) = match _e with
  | Ternary (e1, e2, e3) -> 
    begin match ctx ==> e1 with
      | VBool true -> ctx ==> e2
      | VBool false -> ctx ==> e3
      | VInt _ | VClosure _ | VRec _ as v -> crash ctx (BadGuard (show_val v)) _sp
    end
  | Apply (e1, e2) -> 
    (* get parameter type *)
    let t_arg = match uget (_3 e1) with
      | S.MFun (i, _) -> i
      | _ -> crash ctx (ApplicationNonArrow (Show.ty (_3 e1))) _sp in
    (* if param type > arg type, monomorphize *)
    let e1 = 
      if _3 e2 = t_arg then e1
      else
        let e0 = deepcopy e1 in
        let open Unify in
        (fun[@warning "-8"] (S.MFun (i, _)) -> i) (uget (_3 e0)) =? t_arg;
        e0 in
    (* evaluate applicand *)
    let (ctx', param, body) = match ctx ==> e1 with
      | VClosure (c, p, e, _) -> c, p, e
      | _ -> crash ctx ApplicationNonLambda _sp in
    (* put argument in context and evaluate body *)
    case ctx' param (ctx ==> e2) ==> body
  | Arithmetic (e1, op, e2) -> begin match ctx ==> e1, ctx ==> e2 with
    | VInt i, VInt j -> VInt ((match op with
      | Add -> (+)
      | Sub -> (-)
      | Mul -> ( * )
      | Div -> ( / )
      | Mod -> (fun x y -> (if x > y then Fun.id else Int.neg) (x mod y))) i j)
    | _ -> crash ctx AddingNonIntegers _sp end
  | Comparative (e1, op, e2) -> begin match ctx ==> e1, ctx ==> e2 with
    | VInt i, VInt j -> VBool ((match op with
      | Eq -> (=)
      | Ne -> (<>)
      | Gt -> (>)
      | Lt -> (<)
      | Ge -> (>=)
      | Le -> (<=)) i j)
    | _ -> crash ctx ComparingNonIntegers _sp end
  | Logical (e1, op, e2) -> (match op with
    | And -> (match ctx ==> e1 with
      | VBool true -> ctx ==> e2
      | VBool false -> VBool false
      | _-> crash ctx AndLeftOpNonbool _sp
      )
    | Or -> (match ctx ==> e1 with
      | VBool true -> VBool true
      | VBool false -> ctx ==> e2
      | _ -> crash ctx OrLeftOpNonbool _sp)
  )
  | Not e -> (match ctx ==> e with
    | VBool b -> VBool (not b)
    | _ -> crash ctx NegateBool _sp)
  | Record (e1, op, e2) -> (match ctx ==> e1, ctx ==> e2 with
    | VRec r1, VRec r2 -> (match op with
      | Concatenate -> VRec (Dict.union (fun _ _ x -> Some x) r1 r2)
      | Intersect -> VRec (Dict.merge (fun _ -> function
        | Some _ -> Fun.id
        | None -> Fun.const None) r1 r2))
    | _ -> crash ctx RecOpNonproduct _sp)
  | Project (e, s) -> (match ctx ==> e with
    | VRec r -> (match Dict.find_opt s r with
      | Some v -> v
      | None -> crash ctx (ProjectAbsentField s) _sp)
    | _ -> crash ctx ProjectNonproduct _sp)
  
  | Binding (s, e1, e2) -> add_rec ctx s e1 _t ==> e2
  | Abstract (p, e) -> VClosure (ctx, p, e, _t)
  | RecordCon asgns -> VRec (Dict.of_list (List.map (T2.map2 ((==>) ctx)) asgns))
  | IntLit i -> VInt i
  | BoolLit b -> VBool b
  | Id s -> (match Dict.find_opt s ctx with
    | Some lazy v -> v
    | None -> failwith ("Unbound variable [" ^ s ^ "]."))

and case ctx (p, _sp, _) = match p with
  | Param s -> fun v -> Dict.add s (lazy v) ctx
  | RecPat asgns -> begin function
      | VRec d -> List.fold_left (fun c (s, p') -> Dict.find_opt s d |> function
        | Some x -> case c p' x
        | None -> crash ctx (MatchAbsentField s) _sp) ctx asgns
      | _ -> crash ctx MatchNonrecordOnRecord _sp
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
            | _ -> crash ctx MatchNonrecordOnCat _sp
          end
          | _ -> crash ctx MiscBadRecord _sp
        end
      | _ -> crash ctx MatchCatNonsubproducts _sp
    end

and eval_cases ctx ps es = 
  List.fold_left (fun c (p, e) -> case c p (c ==> e)) ctx (List.combine ps es)

and add_rec c s e t : value Lazy.t Dict.t = match uget t with
  | S.MFun _ -> 
    let rec c' = lazy (Dict.add s (lazy (Lazy.force c' ==> e)) c) in
    Lazy.force c'
  | _ -> Dict.add s (lazy (c ==> e)) c

let eval ctx defs = 
  List.fold_left (fun c -> function
    | (s, e), _, t -> add_rec c s e t
  ) ctx defs
