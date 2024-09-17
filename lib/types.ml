open! Batteries

type gen = 
  | Mono
  | Poly of int Set.t

type atom = 
  | ABool | AInt
  | ARec of string * bool
  | AFun of bool * bool

module Tau_constant = struct

  type t = atom Set.t Set.t
  let zero = Set.empty
  let one = Set.(singleton empty)
  let is_zero = Set.is_empty
  let is_one s = Set.is_singleton s && Set.is_empty (Set.choose s)

  let add = Set.sym_diff
  let summate f s = Set.fold (f %> add) s Set.empty
  let recomb s = 
    let bool_part, int_part, rec_part, fun_part = 
      Set.fold (fun e (bool_part, int_part, rec_part, fun_part as st) -> 
        match e with
        | ABool -> true, int_part, rec_part, fun_part
        | AInt -> bool_part, true, rec_part, fun_part
        | ARec (x, b1) -> begin match Map.find_opt x rec_part with
          | Some true -> st
          | Some false
          | None -> bool_part, int_part, Map.add x b1 rec_part, fun_part
        end
        | AFun (b1, b2) -> bool_part, int_part, rec_part, 
          begin match fun_part with
            | Some (f1, f2) -> Some (b1 || f1, b2 || f2)
            | None -> Some (b1, b2)
          end
      ) s (false, false, Map.empty, None) in
    let a1 = 
      Map.foldi (fun x b -> Set.add (ARec (x, b))) rec_part Set.empty in
    let a2 = match fun_part with
      | Some (b1, b2) -> Set.add (AFun (b1, b2)) a1
      | None -> a1 in
    let a3 = if bool_part then Set.add ABool a2 else a2 in
    if int_part then Set.add AInt a3 else a3
  let inter e1 e2 = recomb (Set.union e1 e2)
  let mul s = summate (fun e -> Set.map (inter e) s)
  
  let atom_to_string = function
    | ABool -> "bool" | AInt -> "int"
    | ARec (x, b) -> Printf.sprintf "{%s:%d}" x (Bool.to_int b)
    | AFun (b1, b2) -> 
      Printf.sprintf "(%d->%d)" (Bool.to_int b1) (Bool.to_int b2)
  let term_to_string t = 
    if Set.is_empty t then "1"
    else String.concat " " (List.map atom_to_string (Set.to_list t))
  let to_string a = match Set.cardinal a with
    | 0 -> "0"
    | 1 -> term_to_string (Set.choose a)
    | _ -> 
      String.concat " + " (List.map term_to_string (Set.to_list a))
      |> Printf.sprintf "(%s)"

end

module Tau = Ubool.Make(Tau_constant)

module Unify = struct
  open Uref
  let simplify r = uset r (Tau.simplify (uget r))
  let (=?) = Tau.unify

  let mk_cache () = Hashtbl.create 16, Hashtbl.create 16
  let generalize ?(tbl=Hashtbl.create 16) w t0 = 
    let cache x = 
      let nu = uref (Tau.Var (!Common.level, unique ())) in
      Hashtbl.add tbl x nu;
      nu in
    let rec gen t = match uget t with
      | Tau.Var (_, i) when Set.mem i w -> t
      | Tau.Var (_, i) -> cache i
      | Expr e -> 
        uref (Tau.Expr (List.map (Tuple2.map2 (List.map gen)) e)) in
  gen t0

  let rec bound t0 = match uget t0 with
    | Tau.Var (m, i) when m <= !Common.level -> Set.singleton i
    | Tau.Var _ -> Set.empty
    | Expr bs -> 
      List.fold_left (fun a (_, ts) -> 
        Set.union a (List.fold_left (fun y z -> 
          Set.union y (bound z)) Set.empty ts)) Set.empty bs
  
  let deepcopy tbl t0 = generalize ~tbl Set.empty t0

end

module Show = struct

  let print_ty out t = Unify.simplify t; Tau.pretty_anf out t
  let ty ty = 
    let s = IO.output_string () in
    print_ty s ty;
    IO.close_out s

end

let bconst c = Set.singleton (Set.singleton c)
let const0 c = Tau.uconst (bconst c)
let brec x t = 
  let t_f = Tau.uconst (bconst (ARec (x, false))) in
  let t_t = Tau.uconst (bconst (ARec (x, true))) in
  Tau.(add_t (mul_t (add_t t_f t_t) t) t_f)

let bfun t1 t2 = 
  let mk_t b1 b2 = Tau.uconst (bconst (AFun (b1, b2))) in
  let t_ff = mk_t false false in
  let t_tf = mk_t true false in
  let t_ft = mk_t false true in
  let t_tt = mk_t true true in
  let ( * ) = Tau.mul_t in
  let ( + ) = Tau.add_t in
  t1 * t2 * (t_ff + t_ft + t_tf + t_tt)
  + t1 * (t_tf + t_ff)
  + t2 * (t_ft + t_ff)
  + t_ff





