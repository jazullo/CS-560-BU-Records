open Batteries
open Uref

exception Err

module type Constant = sig
  type t

  val add : t -> t -> t
  val mul : t -> t -> t
  val zero : t
  val one : t

  val is_zero : t -> bool
  val is_one : t -> bool
  val to_string : t -> string
end

module Make(C : Constant) = struct
  module Const = C
  
  type t = _t uref
  and _t = 
    | Var of int * int  (* level, uid *)
    | Expr of (C.t * t list) list
  
  let one = [C.one, []]
  let const c = [c, []]
  let var v = [C.one, [v]]
  
  let uvar lvl i = uref (Var (lvl, i))
  let uexpr e = uref (Expr e)
  let uconst c = uref (Expr (const c))
  let fresh () = uvar !Common.level (unique ())
  let getvar u = 
    let[@warning "-8"] (Var (_, i)) = uget u in i
  let getexpr u = match uget u with
    | Expr e -> e
    | Var _ -> var u
  
  let mul e1 e2 = 
    List.map
      (fun ((c1, t1), (c2, t2)) -> C.mul c1 c2, t1 @ t2)
      (List.cartesian_product e1 e2)
  
  let add_t u v = match uget u, uget v with
    | Expr [], _ -> v
    | _, Expr [] -> u
    | Var _, Expr e -> uexpr (var u @ e)
    | Expr e, Var _ -> uexpr (var v @ e)
    | Expr e1, Expr e2 -> uexpr (e1 @ e2)
    | Var _, Var _ -> uexpr (var u @ var v)
  
  let mul_t u v = match uget u, uget v with
    | Expr [coeff, []], _ when C.is_one coeff -> v
    | _, Expr [coeff, []] when C.is_one coeff -> u
    | Var _, Expr e -> 
      uexpr (List.map (Tuple2.map2 (List.cons u)) e)
    | Expr e, Var _ -> 
      uexpr (List.map (Tuple2.map2 (List.cons v)) e)
    | Expr e1, Expr e2 -> uexpr (mul e1 e2)
    | Var _, Var _ -> uexpr [C.one, [u; v]]
  
  let map_expr f = function
    | Var _ as v -> v
    | Expr e -> Expr (f e)
  
  let upd_expr f x = uset x (f (uget x)); x
  
  let rec distribute e = 
    List.fold_left (fun expr_acc (coeff, vars) ->  
      let flatvars = List.map (upd_expr (map_expr distribute)) vars in
      let newterms = 
        getexpr (List.fold_left mul_t (uconst coeff) flatvars) in
      newterms @ expr_acc
    ) [] e
  
  let compare_with f x y = compare (f x) (f y)
  
  let elim = 
    List.map (Tuple2.map2 (List.sort_uniq (compare_with uget)))
    %> List.sort (compare_with snd)
    %> List.fold_left (function
      | [] -> List.singleton
      | (c, t) :: ts as acc -> fun (const, vars as term) -> 
        if t = vars then
          let c' = C.add c const in
          if C.is_zero c' then ts
          else (c', t) :: ts
        else term :: acc
    ) []
    %> List.filter (fst %> C.is_zero %> not)
  
  let simp = distribute %> elim %> List.rev
  let simplify = map_expr simp

  open Printf

  let pretty_term_anf out = function
    | coeff, [] when C.is_one coeff -> fprintf out "%s" C.(to_string one)
    | coeff, [] -> fprintf out "%s" (C.to_string coeff)
    | coeff, v :: vars when C.is_one coeff -> 
      fprintf out "b%d" (getvar v);
      List.iter (getvar %> fprintf out " b%d") vars
    | coeff, vars -> 
      fprintf out "%s" (C.to_string coeff);
      List.iter (getvar %> fprintf out " b%d") vars

  let pretty_anf out = uget %> map_expr simp %> function
    | Var (_, i) -> fprintf out "b%d" i
    | Expr [] -> fprintf out "%s" C.(to_string zero)
    | Expr (t :: ts) -> 
      pretty_term_anf out t;
      List.iter (fun x -> fprintf out " + "; pretty_term_anf out x) ts

  let string_anf u = 
    let out = IO.output_string () in
    pretty_anf out u;
    IO.close_out out
  
  let print_anf u = print_endline (string_anf u)
  
  let[@warning "-8"] smallterm (x :: xs) = 
    List.fold_left (fun t t' -> 
      if List.compare_lengths t t' = 1 then t'
      else t
    ) x xs
  
  let counts e candidates = 
    List.concat_map snd e
    |> List.fold_left (fun c t -> 
      Map.modify_opt t (Option.map succ) c
    ) (List.enum candidates |> Enum.map (fun x -> x, 0) |> Map.of_enum)
    |> Map.enum |> List.of_enum |> List.sort (fun x y -> compare (snd x) (snd y))
    |> List.map fst
  
  let select_var ~vars e = 
    List.map snd e |> List.filter (Fun.negate List.is_empty)
    |> smallterm |> counts e
    |> List.find_opt (getvar %> Option.map_default Hashtbl.mem (fun _ -> true) vars)
  
  let factor u = 
    List.partition_map (fun (coeff, vars) -> 
      match[@warning "-8"] List.partition (Uref.equal u) vars with
      | [], full -> Right (coeff, full)
      | [_], part -> Left (coeff, part)
    )
  
  let rec solve ~vars e0 = match simp e0 with
    | [] -> ()
    | [_, []] -> raise Err
    | e -> match select_var ~vars e with
      | None -> raise Err
      | Some u -> 
        let[@warning "-8"] (Var (lvl, uid)) = uget u in
        let t1, t2 = factor u e in
        Option.may (fun vs -> Hashtbl.remove vs uid) vars;
        solve ~vars (mul t2 (one @ t1));
        let lvl', uid' = !Common.level, unique () in
        let u' = uvar (min lvl lvl') uid' in
        Option.may (fun vs -> Hashtbl.add vs (getvar u') u') vars;
        uset u (Expr (simp (t2 @ mul (var u') (one @ t1))))
  
  let rec unify ?(vars=None) r = unite ~sel:(curry @@ function
    | Var (l1, i1), Var (l2, _) -> 
      Option.may (fun vs -> Hashtbl.remove vs i1) vars;
      Var (min l1 l2, i1)
    | (Var (_, i1) as v, (Expr e as x) | (Expr e as x), (Var (_, i1) as v)) -> 
      List.(find_map_opt (snd %> find_opt (Uref.uget %> (=) v))) e |> (function
        | Some u -> u |> var %> uexpr %> unify ~vars (uref x)
        | None -> Option.may (fun vs -> Hashtbl.remove vs i1) vars); x
    | Expr e1 as x, (Expr e2 as y) -> 
      try solve ~vars (e1 @ e2); x with
      | Err -> raise @@ Common.UnifError (Printf.sprintf 
        "Incompatible Set Types <%s> and <%s>."
        (string_anf (uref x))
        (string_anf (uref y)))
  ) r

end
