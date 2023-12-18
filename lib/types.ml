open! Batteries
open Uref
open Ubool

module Dict = Map.Make(String)

type mode = Fin | Inv  (* finite or inverted *)

module rec S : sig

  (* internal representation of types *)
  type t = _t uref
  and _t = 
    | MVar of int   (* polymorphic type variables *)
    | MLit of mlit
    | MFun of t * t
    | TRec of recty  (* polymorphic records *)

  and mlit = MInt | MBool

  and recty = Free.t

end = S

and Free : sig  (* Boolean unifier for free boolean rings *)
  type t = _t uref
  and _t = 
    | Var of int
    | Expr of ((mode * S.t Dict.t) * t list) list  (* FBRs implemented as dicts *)
  
  val unify : t -> t -> unit
  val simplify : _t -> _t
  val fresh : unit -> t
  val add_t : t -> t -> t
  val mul_t : t -> t -> t
  val uconst : mode * S.t Dict.t -> t
  val pretty_anf : 'a BatInnerIO.output -> t -> unit
  val print_anf : t -> unit
end = Make(struct
  (* Infinite Free Boolean Rings *)
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
    | Fin, Inv -> Inv, diff    r1 r2
    | Inv, Fin -> Inv, diff    r2 r1

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
  val generalize : S.t -> S.t
end = struct

  let simplify r = uset r (Free.simplify (uget r))

  (* syntactic unification *)
  let rec (=?) r = r |> unite ~sel:begin curry @@ function
    | S.MVar v as w, S.MVar u when v = u -> w
    | S.MVar v, u | u, S.MVar v -> occurs v u; u
    | S.MLit _ as u, v when u = v -> u
    | MFun (i1, o1) as f, MFun (i2, o2) -> 
      i1 =? i2;
      o1 =? o2;
      f
    | TRec r1 as r, TRec r2 -> 
      Free.unify r1 r2;
      (* simplify r1; *)
      r
    | _ -> raise (Common.UnifError "Cannot unify distinct concrete types.")
  end

  and occurs v = function
    | S.MVar u when v = u -> 
      raise (Common.UnifError "Cannot unify variable with term that contains it.")
    | MFun (i, o) -> occurs v (uget i); occurs v (uget o)
    | TRec r -> 
      simplify r;
      begin match uget r with
      | Var _ -> ()
      | Expr bs -> 
        List.iter (fst %> snd %> Dict.iter (fun _ -> uget %> occurs v)) bs
      end
    | _ -> ()
  
  let generalize t0 = 
    let tbl = Hashtbl.create 16 in
    let tbl_rec = Hashtbl.create 16 in
    let cache x = 
      Hashtbl.find_option tbl x |> Option.default_delayed @@ fun () -> 
        let nu = uref @@ S.MVar (unique ()) in
        Hashtbl.add tbl x nu;
        nu in
    let cache_rec x = 
      Hashtbl.find_option tbl_rec x |> Option.default_delayed @@ fun () -> 
        let nu = uref @@ Free.Var (unique ()) in
        Hashtbl.add tbl_rec x nu;
        nu in
    let rec gen t = 
      match uget t with
      | S.MVar i -> cache i
      | MLit _ -> t
      | MFun (i, o) -> uref (S.MFun (gen i, gen o))
      | TRec r -> uref (S.TRec (gen_rec r))
    and gen_rec r = 
      let gen_term ((mode, coeff), vars) = 
        (mode, Dict.map gen coeff), List.map gen_rec vars in
      match uget r with
      | Free.Var i -> cache_rec i
      | Expr e -> uref @@ Free.Expr (List.map gen_term e) in
    gen t0
  
end

and Show : sig
  val print_ty : 'a BatInnerIO.output -> S.t -> unit
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
    | S.MVar i -> fprintf out "a%d" i
    | MLit MInt -> fprintf out "int"
    | MLit MBool -> fprintf out "bool"
    | MFun _ as e -> 
      fprintf out "(";
      print__t_fst out e;
      fprintf out ")"
    | S.TRec r -> print_rec_ty out r
  
  and print_rec_ty out r = 
    Unify.simplify r;
    fprintf out "<";
    Free.pretty_anf out r;
    fprintf out ">"
  
  let print_ty = print_t_fst

end
