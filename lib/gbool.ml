(* B-generalization *)
open! Batteries

module B = struct
  open Types
  open Uref
  include Free
  let simplify = Unify.simplify
  let add = add_t
  let mul = mul_t
  let zero = uref (Expr [(Fin, Dict.empty), []])
  let one = uref (Expr [(Inv, Dict.empty), []])
  let uexpr = function
    | [] -> zero
    | e -> 
      let t = uref (Expr e) in
      Unify.simplify t; t
  let is_zero t = 
    Unify.simplify t;
    Uref.uget t = Uref.uget zero || Uref.uget t = Uref.uget (uexpr [])
  let is_one t = 
    let t3 = add t one in
    is_zero t3
  let to_string rho = 
    let s = IO.output_string () in
    Show.print_rec_ty s rho;
    IO.close_out s
  
  let bmatch v = uget %> function
    | Expr e -> 
      let e1, e2 = Tuple2.mapn uexpr (Free.factor (v) e) in
      Unify.(simplify e1; simplify e2); (e1, e2)
    | Var _ as v_ -> 
      if v_ = uget v then v, zero
      else failwith "internal error: scrutinee variable absent from row type"

  let eq x y = is_zero (add x y)
  let mem x = List.exists (eq x)
  
  let vars t = match uget t with
    | Var _ -> [t]
    | Expr t_ -> List.fold_left (fun acc (_, bases) -> 
      List.fold_left (fun a v -> 
        if mem v a then a
        else v :: a) acc bases) [] t_
  
  let replace v t t0 = match uget t0 with
    | Var _ when Uref.equal v t0 -> t
    | Var _ -> v
    | Expr e -> uexpr (List.map (Tuple2.map2 (List.map (fun v0 -> 
      match uget v0 with
      | Var _ when Uref.equal v0 t0 -> t
      | Var _ -> v0
      | Expr _ -> failwith "Non-normal array"))) e)
  
  let project vars t = match uget t with
    | Var _ when mem t vars -> t
    | Var _ -> one
    | Expr e -> 
      uexpr (List.map (Tuple2.map2 (List.filter (fun v -> mem v vars))) e)
  
  let minlvl v1 v2 = match[@warning "-8"] uget v1, uget v2 with
    | Var (lvl1, _), Var (lvl2, _) -> if lvl2 > lvl1 then v2 else v1
end

let eq x y = B.(is_zero (add x y))
let mem x = List.exists (eq x)

open Types
open Uref

let getvar = uget %> function[@warning "-8"] Free.Var (_, i) -> i

(* Main algorithm *)
let rec gen dense sparse arr = match dense with
  | [] -> () | v1 :: dense_tail -> 
    dense_tail @ sparse |> List.iter @@ fun v2 -> 
      let ps = Free.[fresh (); fresh (); fresh ()] in
      let ht = Hashtbl.of_list (List.map (fun p -> getvar p, p) ps) in
      begin try
        let a = arr |> Array.map @@ fun r -> 
          let p0 = Free.fresh () in
          Hashtbl.add ht (getvar p0) p0;
          let t1, t2 = B.bmatch v1 r in
          let t3, t4 = B.bmatch v2 t1 in
          let t5, t6 = B.bmatch v2 t2 in
          let u = B.mul p0 %> Free.unify ~vars:ht in
          List.iter2 u ps [t3; t4; t5];
          B.(add (mul p0 (B.minlvl v1 v2)) t6) in
        Hashtbl.iter (fun _ u -> uset u (uget B.zero)) ht;
        Array.blit a 0 arr 0 (Array.length a)
      with Common.UnifError _ -> () end;
    gen dense_tail sparse arr

(* Mapping types to and from the AST *)
open Free

type frozen_t = 
  | FVar of int | FInt | FBool
  | FFun of frozen_t * frozen_t
  | FRec of frozen_rec
and frozen_rec = ((mode * (string * frozen_t) list) * int list) list

let getvar = uget %> function[@warning "-8"] Var (_, i) -> i

let rec freeze t = match uget t with
  | S.MVar (_, i) -> FVar i
  | MLit MInt -> FInt | MLit MBool -> FBool
  | MFun (i, o) -> FFun (freeze i, freeze o)
  | TRec r -> freeze_recty r
and freeze_recty r = match Free.simplify (uget r) with
  | Var (_, i) -> FRec [(Inv, []), [i]]
  | Expr e -> 
    FRec (List.map Tuple2.(map (map Fun.id (Dict.to_list %> 
      List.map (map2 freeze))) (List.map getvar)) e)

let many ctx tau = 
  let ht = Hashtbl.create 32 in
  let idx = ref (-1) in
  let nu () = incr idx; !idx in
  let intern x = 
    Hashtbl.find_option ht x |> Option.default_delayed (fun () -> 
      let i = nu () in
      Hashtbl.add ht x i; i) in
  let infroz = freeze_recty %> intern in
  let rec gather_rows t = match uget t with
    | S.MVar _ | MLit _ -> ()
    | MFun (i, o) -> gather_rows i; gather_rows o
    | TRec r -> 
      infroz r |> ignore; 
      match uget r with
      | Free.Var _ -> ()
      | Free.Expr e -> 
        List.iter (fst %> snd %> Dict.values %> Enum.iter gather_rows) e in
  gather_rows tau;
  Cyclic.vmap (fst %> gather_rows) ctx |> ignore;
  let arr = Array.init (Hashtbl.length ht) (fun _ -> B.zero) in
  let rec set_rows t = match uget t with
    | S.MVar _ | MLit _ -> ()
    | MFun (i, o) -> set_rows i; set_rows o
    | TRec r -> 
      arr.(Hashtbl.find ht (freeze_recty r)) <- r;
      match uget r with
      | Var _ -> ()
      | Expr e -> 
        List.iter (fst %> snd %> Dict.values %> Enum.iter set_rows) e in
  set_rows tau;
  Cyclic.vmap (fst %> set_rows) ctx |> ignore;
  let rec row_vars t = match uget t with
    | S.MVar _ | MLit _ -> Map.empty
    | MFun (i, o) -> Map.union (row_vars i) (row_vars o)
    | TRec r -> match uget r with
      | Var (_, i) -> Map.singleton i r
      | Expr e -> List.fold_left (fun a ((_, d), vs) -> 
          let go_rec _ = row_vars %> Map.union in
          let addvar ws v = Map.add (getvar v) v ws in
          Dict.fold go_rec d (List.fold_left addvar a vs)) Map.empty e in
  let tau_vars = row_vars tau in
  let ctx_vars = 
    List.map (snd %> fst %> row_vars) (Cyclic.to_list ctx)
    |> List.fold_left Map.union Map.empty in
  let ctx_types_reduced = Map.filter (fun i _ -> not (Map.mem i tau_vars)) ctx_vars in
  let to_list = Map.values %> List.of_enum in
  gen (to_list tau_vars) (to_list ctx_types_reduced) arr;
  let rec reconstruct t = match uget t with
    | S.MVar _ | MLit _ -> t
    | MFun (i, o) -> uref (S.MFun (reconstruct i, reconstruct o))
    | TRec r -> S.TRec (arr.(Hashtbl.find ht (freeze_recty r))) |> uref in
  Cyclic.vmap (Tuple2.map1 (fun r -> uset r (uget (reconstruct r)))) ctx |> ignore; 
  reconstruct tau
