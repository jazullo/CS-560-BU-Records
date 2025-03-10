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

let gen dense sparse arr = 
  let rec go1 d1 s1 = match d1 with
    | [] -> () | v1 :: d2 -> 
      d2 @ s1 |> List.iter @@ fun v2 -> 
        let v, p0 = B.minlvl v1 v2, Free.fresh () in
        let p3, p4, p5 as ps = Free.(fresh (), fresh (), fresh ()) in
        let h3, h4, h5 as hs = 
          Tuple3.mapn (fun p -> Hashtbl.of_list [getvar p, p]) ps in
        begin try 
          let a = arr |> Array.map @@ fun r -> 
            let t1, t2 = B.bmatch v1 r in
            let t3, t4 = B.bmatch v2 t1 in
            let t5, t6 = B.bmatch v2 t2 in
            Free.unify ~vars:h3 (B.mul p0 p3) t3;
            Free.unify ~vars:h4 (B.mul p0 p4) t4;
            Free.unify ~vars:h5 (B.mul p0 p5) t5;
            B.(add (mul p0 v) t6) in
          let sub_zero _ u = uset u (uget B.zero) in
          ignore (Tuple3.mapn (Hashtbl.iter sub_zero) hs);
          Array.blit a 0 arr 0 (Array.length a)
        with Common.UnifError _ -> () end;
      go1 d2 s1 in
  go1 dense sparse
