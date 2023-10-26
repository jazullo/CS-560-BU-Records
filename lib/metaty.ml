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

  and recty = Free.ubool

end = S

and Free : sig  (* Boolean unifier for free boolean rings *)
  type boolean = boolean_ ref
  and boolean_ = 
    | BExpr of boolean list list
    | BVar of int
    | BConst of (mode * S.t Dict.t)  (* FBRs implemented as dicts *)
  
  type ubool = boolean list list uref

  val unify : ubool -> ubool -> unit
  val bfresh : unit -> ubool
end = Make(struct
  (* Infinite Free Boolean Rings *)
(* Fin: S.t Dict.t is the record with keys = string (tags) and values = S.t (types) 
   Inv: Dict is complemented (the tags of the record are _all other strings_ ) *)
  type t = mode * S.t Dict.t

  let zero = Fin, Dict.empty
  let one  = Inv, Dict.empty

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

  let and_const (m1, r1) (m2, r2) = match m1, m2 with
    | Fin, Fin -> Fin, inter r1 r2
    | Inv, Inv -> Inv, union r1 r2
    | Fin, Inv -> Fin, diff  r1 r2
    | Inv, Fin -> Inv, diff  r2 r1
  let xor_const (m1, r1) (m2, r2) = match m1, m2 with
    | Fin, Fin -> Fin, symdiff r1 r2
    | Inv, Inv -> Fin, symdiff r1 r2
    | Fin, Inv -> Inv, diff    r1 r2
    | Inv, Fin -> Inv, diff    r2 r1

  let to_string = failwith "todo"  (* convert to DNF and print *)
end)

and Unify : sig
  val (=?) : S.t -> S.t -> unit
end = struct

  (* syntactic unification *)
  let rec (=?) r = r |> unite ~sel:begin curry @@ function
    | S.MVar _, u | u, S.MVar _ -> u (* Add an occurs check *)
    | S.MLit _ as u, v when u = v -> u
    | MFun (i1, o1) as f, MFun (i2, o2) -> 
      i1 =? i2;
      o1 =? o2;
      f
    | TRec r1 as r, TRec r2 -> 
      Free.unify r1 r2;
      r
    | _ -> failwith "Cannot unify distinct concrete types"
  end

end
