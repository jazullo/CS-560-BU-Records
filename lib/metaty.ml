open! Batteries
open Uref
open Ubool

module Dict = Map.Make(String)

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
    | BConst of (bool * S.t Dict.t)  (* FBRs implemented as dicts *)
  
  type ubool = boolean list list uref

  val unify : ubool -> ubool -> unit
  val bfresh : unit -> ubool
end = Make(struct
  (* Infinite Free Boolean Rings *)
(* bool=false ==> 
     S.t Dict.t is the record with keys = string (tags) and values = S.t (types) 
   bool=true ==> 
     Dict is complemented (the tags of the record are _all other strings_ ) *)
  type t = bool * S.t Dict.t

  let zero = false, Dict.empty
  let one = true, Dict.empty

  (* important helpers *)
  let usnd _ c1 c2 = 
    Unify.(=?) c1 c2;
    c2
  let liftA2 f o1 o2 = match o1, o2 with
    | Some x1, Some x2 -> Some (f x1 x2)
    | None, _ | _, None -> None
  let[@warning "-32"] inter r = Dict.merge (fun s -> liftA2 (usnd s)) r
  let[@warning "-32"] union r = Dict.union (fun s r1 r2 -> Some (usnd s r1 r2)) r
  
  (* let diff r1 r2 = Dict.filter (fun s c2 -> Dict.find_opt s r2
    |> Option.default_delayed (fun () -> )) r1 *)

  (* let and_const (m1, r1) (m2 r2) = match m1, m2 with
    | false, false -> false, Dict.merge (fun _ _ c -> c) r1 r2
    | true, true -> true, Dict.union (fun _ _ c -> c) r1 r2 *)
  let and_const = failwith "todo"
  let xor_const = failwith "todo"  (* "" *)

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
