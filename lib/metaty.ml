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
    Unify.(=?) c1 c2;
    c2
  let liftA2 f o1 o2 = match o1, o2 with
    | Some x1, Some x2 -> Some (f x1 x2)
    | None, _ | _, None -> None
  let inter r = Dict.merge (fun _ -> liftA2 usnd) r
  let union r = Dict.union (fun _ r1 r2 -> Some (usnd r1 r2)) r
  let diff r1 r2 = Dict.filter (fun s _ -> not @@ Dict.mem s r2) r1
  let symdiff r = Dict.merge begin fun _ r1 r2 -> match r1, r2 with
    | Some _ as r3, None | None, (Some _ as r3) -> r3
    | Some _, Some _ | None, None -> None
  end r

  let add (m1, r1) (m2, r2) = match m1, m2 with
    | Fin, Fin -> Fin, inter r1 r2
    | Inv, Inv -> Inv, union r1 r2
    | Fin, Inv -> Fin, diff  r1 r2
    | Inv, Fin -> Fin, diff  r2 r1
  let mul (m1, r1) (m2, r2) = match m1, m2 with
    | Fin, Fin -> Fin, symdiff r1 r2
    | Inv, Inv -> Fin, symdiff r1 r2
    | Fin, Inv -> Inv, diff    r1 r2
    | Inv, Fin -> Inv, diff    r2 r1

  let to_string = failwith "todo"  (* convert to DNF and print *)
end)

and Unify : sig
  val (=?) : S.t -> S.t -> unit
  val simplify : Free.t -> unit
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
    | _ -> failwith "Cannot unify distinct concrete types"
  end

  and occurs v = function
    | S.MVar u when v = u -> 
      failwith "Cannot unify variable with term that contains it"
    | MFun (i, o) -> occurs v (uget i); occurs v (uget o)
    | TRec r -> 
      simplify r;
      begin match uget r with
      | Var _ -> ()
      | Expr bs -> 
        List.iter (fst %> snd %> Dict.iter (fun _ -> uget %> occurs v)) bs
      end
    | _ -> ()

end

and Show : sig

end = struct

  open Printf

  let rec print_t_fst out = uget %> print__t_fst out
  and print__t_fst out = function
    | S.MFun (i, o) -> 
      print_t out i;
      fprintf out " -> ";
      print_t out o
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
    | S.TRec r -> print_rec_var out r
  
  and print_rec_var out r = 
    Unify.simplify r;
    match uget r with
    | Free.Var i -> fprintf out "b%d" i
    | Free.Expr _ -> failwith "todo"

end
