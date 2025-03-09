open! Batteries
open Uref
open Ubool

module Dict = Map.Make(String)
module Universe = Set.Make(Int)

type mode = Fin | Inv  (* finite or inverted *)
type gen = 
  | Mono
  | Poly of Universe.t

module rec S : sig

  (* internal representation of types *)
  type t = _t uref
  and _t = 
    | MVar of int * int   (* polymorphic type variables *)
    | MLit of mlit
    | MFun of t * t
    | TRec of recty  (* polymorphic records *)

  and mlit = MInt | MBool

  and recty = Free.t

end = S

and Free : sig  (* Boolean unifier for infinite boolean rings *)
  type t = _t uref
  and _t = 
    | Var of int * int
    | Expr of ((mode * S.t Dict.t) * t list) list  (* as dicts with complement flag *)
  
  val unify : t -> t -> unit
  val simplify : _t -> _t
  val fresh : unit -> t
  val add_t : t -> t -> t
  val mul_t : t -> t -> t
  val uconst : mode * S.t Dict.t -> t
  val pretty_anf : 'a BatInnerIO.output -> t -> unit
  val print_anf : t -> unit
  module Const : Constant with type t := mode * S.t Dict.t
  val factor : 
    t -> ((mode * S.t Dict.t) * t list) list -> 
    ((mode * S.t Dict.t) * t list) list * ((mode * S.t Dict.t) * t list) list
end = Make(struct
  (* Infinite Boolean Rings (Free BRs of a countably infinite set) *)
(* Fin: S.t Dict.t is the record with keys = string (tags) and values = S.t (types) 
   Inv: Dict is complemented (the tags of the record are _all other strings_ ) *)
  type t = mode * S.t Dict.t

  let zero = Fin, Dict.empty
  let one  = Inv, Dict.empty

  let is_zero = function
    | Fin, d -> Dict.is_empty d
    | _ -> false
  
  let is_one = function
    | Inv, d -> Dict.is_empty d
    | _ -> false

  (* important helpers *)
  let usnd c1 c2 = 
    Unify.(c1 =? c2);
    c2
  let liftA2 f o1 o2 = match o1, o2 with
    | Some x1, Some x2 -> Some (f x1 x2)
    | None, _ | _, None -> None
  let inter r = Dict.merge (fun _ -> liftA2 usnd) r
  let union r = Dict.union (fun _ r1 r2 -> Some (usnd r1 r2)) r
  let diff r = Dict.merge begin fun _ r1 r2 -> match r1, r2 with
    | Some _ as r3, None -> r3
    | Some c1, Some c2 -> Unify.(c1 =? c2); None
    | None, None | None, (Some _) -> None
  end r
  let symdiff r = Dict.merge begin fun _ r1 r2 -> match r1, r2 with
    | Some _ as r3, None | None, (Some _ as r3) -> r3
    | Some c1, Some c2 -> Unify.(c1 =? c2); None
    | None, None -> None
  end r

  let mul (m1, r1) (m2, r2) = match m1, m2 with
    | Fin, Fin -> Fin, inter r1 r2
    | Inv, Inv -> Inv, union r1 r2
    | Fin, Inv -> Fin, diff  r1 r2
    | Inv, Fin -> Fin, diff  r2 r1
  let add (m1, r1) (m2, r2) = match m1, m2 with
    | Fin, Fin -> Fin, symdiff r1 r2
    | Inv, Inv -> Fin, symdiff r1 r2
    | Fin, Inv -> Inv, symdiff r1 r2
    | Inv, Fin -> Inv, symdiff r1 r2

  let to_string (mode, d) = 
    let body = IO.output_string () in
    Dict.print
      ~first:"{" ~last:"}" ~sep:", " ~kvsep:" : "
      String.print Show.print_ty body d;
    (match mode with
    | Fin -> ""
    | Inv -> "!"
    ) ^ IO.close_out body
end)

and Unify : sig
  val (=?) : S.t -> S.t -> unit
  val simplify : Free.t -> unit
  val generalize : ?tbl:(int, S.t) Hashtbl.t -> ?tbl_rec:(int, Free.t) Hashtbl.t -> 
    Universe.t -> S.t -> S.t
  val bound : S.t -> Universe.t
  val deepcopy : (int, S.t) Hashtbl.t * (int, Free.t) Hashtbl.t -> S.t -> S.t
  val mk_cache : unit -> ('a, 'b) Hashtbl.t * ('c, 'd) Hashtbl.t
end = struct

  let simplify r = uset r (Free.simplify (uget r))

  (* syntactic unification *)
  let rec (=?) r = r |> unite ~sel:begin curry @@ function
    | S.MVar (n, v), S.MVar (m, u) when v = u -> S.MVar (min n m, u)
    | S.MVar (n, v), u | u, S.MVar (n, v) -> occurs n v u
    | S.MLit _ as u, v when u = v -> u
    | MFun (i1, o1) as f, MFun (i2, o2) -> 
      i1 =? i2;
      o1 =? o2;
      f
    | TRec r1 as r, TRec r2 -> 
      Free.unify r1 r2;
      r
    | l, r -> raise (Common.UnifError (Printf.sprintf
      "Cannot unify distinct concrete types [%s] and [%s]."
      (Show.ty (uref l)) (Show.ty (uref r))))
  end

  and occurs n v = function
    | S.MVar (_, u) when v = u -> 
      raise (Common.UnifError "Cannot unify variable with term that contains it.") 
    | S.MVar (m, u) -> S.MVar (min n m, u)
    | MFun (i, o) -> 
      uset i (occurs n v (uget i)); 
      uset o (occurs n v (uget o));
      MFun (i, o)
    | TRec r -> 
      simplify r;
      begin match uget r with
      | Var _ -> ()
      | Expr bs -> 
        List.iter (fst %> snd %> Dict.iter (fun _ x -> uset x (occurs n v (uget x)))) bs
      end; 
      TRec r
    | r -> r
  
  let mk_cache () = Hashtbl.create 16, Hashtbl.create 16
  
  let generalize ?(tbl=Hashtbl.create 16) ?(tbl_rec=Hashtbl.create 16) w t0 = 
    let cache x = 
      Hashtbl.find_option tbl x |> Option.default_delayed @@ fun () -> 
        let nu = uref @@ S.MVar (!Common.level, unique ()) in
        Hashtbl.add tbl x nu;
        nu in
    let cache_rec x = 
      Hashtbl.find_option tbl_rec x |> Option.default_delayed @@ fun () -> 
        let nu = uref @@ Free.Var (!Common.level, unique ()) in
        Hashtbl.add tbl_rec x nu;
        nu in
    let rec gen t = 
      match uget t with
      | S.MVar (_, i) when Universe.mem i w -> t
      | S.MVar (_, i) -> cache i
      | MLit _ -> t
      | MFun (i, o) -> uref (S.MFun (gen i, gen o))
      | TRec r -> uref (S.TRec (gen_rec r))
    and gen_rec r = 
      let gen_term ((mode, coeff), vars) = 
        (mode, Dict.map gen coeff), List.map gen_rec vars in
      match uget r with
      | Free.Var (_, i) when Universe.mem i w -> r
      | Free.Var (_, i) -> cache_rec i
      | Expr e -> uref @@ Free.Expr (List.map gen_term e) in
    gen t0
  
  let rec bound t0 = match uget t0 with
    | S.MVar (m, i) when m <= !Common.level -> Universe.singleton i
    | MVar _ | MLit _ -> Universe.empty
    | MFun (i, o) -> Universe.union (bound i) (bound o)
    | TRec r -> 
      begin match uget r with
        | Var (m, i) when m <= !Common.level -> Universe.singleton i
        | Var _ -> Universe.empty
        | Expr bs -> 
          List.fold_left (fun a e -> Dict.fold (fun _ t1 -> 
            Universe.union (bound t1)
          ) (snd (fst e)) a) Universe.empty bs
      end
  
  let deepcopy (tbl, tbl_rec) t0 = generalize ~tbl ~tbl_rec Universe.empty t0

  (* let dc_helper x = Tuple2.map2 (Dict.map (Unify.deepcopy x))

  let rec deepcopy_rec x e = uref @@ match uget e with
    | Free.Var (n, v) -> Free.Var (n, v)
    | Expr ts -> Expr (List.map (Tuple2.map (dc_helper x) (List.map (deepcopy_rec x))) ts) *)
  
end

and Show : sig
  val print_ty : 'a BatInnerIO.output -> S.t -> unit
  val ty : S.t -> string
  val print_rec_ty : ?delim:bool -> 'a BatInnerIO.output -> Free.t -> unit
  val recty : Free.t -> string
end = struct

  open Printf

  let rec print_t_fst out = uget %> print__t_fst out
  and print__t_fst out = function
    | S.MFun (i, o) -> 
      print_t out i;
      fprintf out " -> ";
      print_t_fst out o
    | e -> print__t out e
  and print_t out = uget %> print__t out
  and print__t out = function
    | S.MVar (_, i) -> fprintf out "a%d" i
    | MLit MInt -> fprintf out "int"
    | MLit MBool -> fprintf out "bool"
    | MFun _ as e -> 
      fprintf out "(";
      print__t_fst out e;
      fprintf out ")"
    | S.TRec r -> print_rec_ty out r
  
  and print_rec_ty ?(delim=true) out r = 
    Unify.simplify r;
    if delim then fprintf out "<";
    Free.pretty_anf out r;
    if delim then fprintf out ">"
  
  let print_ty = print_t_fst

  let ty ty = 
    let s = IO.output_string () in
    print_ty s ty;
    IO.close_out s
  
  let recty ty = 
    let s = IO.output_string () in
    print_rec_ty s ty;
    IO.close_out s

end

module BGenAux = struct
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

  let union x y = Const.(add (add x y) (mul x y))
  
  let factor_consts t = match uget t with
    | Var _ -> [one], t
    | Expr [] -> [zero], zero  (* technically unreachable *)
    | Expr [_] when is_zero t -> [zero], zero
    | Expr ((h_, _) :: t_) -> 
      let gcd = List.fold_left (Fun.flip (fst %> union)) h_ t_ in
      (match gcd with
        | Fin, _ -> [uconst gcd]
        | Inv, d -> Dict.fold (fun x f acc -> uconst (Inv, Dict.singleton x f) :: acc) d []), 
      uexpr (List.map (Tuple2.map1 Const.(fun coeff -> union coeff (add one gcd))) t_)
  
  let extract_consts = uget %> function[@warning "-8"]
    | Expr [c1, []] -> c1
    | Expr [] -> Const.zero
  let inter_consts t1 t2 = match[@warning "-8"] Tuple2.mapn (List.map extract_consts) (t1, t2) with
    | [Fin, _ as c1], [Fin, _ as c2] -> [uconst (union c1 c2)]
    | ((Inv, _) :: _), ((Inv, _) :: _) -> List.filter (fun c2 -> List.exists (eq c2) t1) t2
    | [Fin, _ as c1], ((Inv, _) :: _) -> List.filter Const.(extract_consts %> mul c1 %> add c1 %> is_zero) t2
    | ((Inv, _) :: _), [Fin, _ as c1] -> List.filter Const.(extract_consts %> mul c1 %> add c1 %> is_zero) t1
    | [], _ -> [one] | _, [] -> [one]
  let diff_consts t1 t2 = match[@warning "-8"] Tuple2.mapn (List.map extract_consts) (t1, t2) with
    | [Fin, _ as c1], [Fin, _ as c2] -> [uconst Const.(mul c2 (add one c1))]
    | ((Inv, _) :: _), ((Inv, _) :: _) -> List.filter (fun c2 -> not (List.exists (eq c2) t1)) t2
    | [Fin, _ as c1], ((Inv, _) :: _ as c2s) -> [uconst (List.fold_left union c1 c2s)]
    | ((Inv, _) :: _), [Fin, _ as c1] -> List.filter Const.(extract_consts %> mul c1 %> is_zero %> not) t1
    | [], _ -> [one] | _, [] -> t1
  
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

module BGen = struct
  module G = Gbool.Make(BGenAux)
  include G
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
  and freeze_recty r = match uget r with
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
    let arr = Array.init (Hashtbl.length ht) (fun _ -> BGenAux.zero) in
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
    let rec row_vars t = 
      match uget t with
      | S.MVar _ | MLit _ -> Map.empty
      | MFun (i, o) -> Map.union (row_vars i) (row_vars o)
      | TRec r -> match uget r with
        | Var (_, i) -> Map.singleton i r
        | Expr e -> List.fold_left (fun a ((_, d), _) -> 
          Dict.fold (fun _ -> row_vars %> Map.union) d a) Map.empty e in
    let tau_vars = row_vars tau in
    let ctx_vars = 
      List.map (snd %> fst %> row_vars) (Cyclic.to_list ctx)
      |> List.fold_left Map.union Map.empty in
    let ctx_types_reduced = Map.merge (fun _ o1 o2 -> match o1, o2 with
      | (Some _ | None), Some _ | None, None -> None
      | Some _ as o, None -> o) ctx_vars tau_vars in
    let to_list = Map.values %> List.of_enum in
    gen (to_list tau_vars) (to_list ctx_types_reduced) arr;
    let rec reconstruct t = match uget t with
      | S.MVar _ | MLit _ -> t
      | MFun (i, o) -> uref (S.MFun (reconstruct i, reconstruct o))
      | TRec r -> S.TRec begin match uget (arr.(Hashtbl.find ht (freeze_recty r))) with
        | Free.Var _ -> r
        | Expr e -> 
          uref (Expr (List.map Tuple2.(map1 (map2 (Dict.map reconstruct))) e))
      end |> uref in
    Cyclic.vmap (Tuple2.map1 reconstruct) ctx, reconstruct tau
end
