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
  type t = bool * S.t Dict.t  (* true flips dict to top, empty fields are treated as variables *)

  let zero = false, Dict.empty
  let one = true, Dict.empty

  let and_const = failwith "todo"  (* implement in terms of (=?) *)
  let xor_const = failwith "todo"  (* "" *)

  let to_string = failwith "todo"  (* convert to DNF! *)
end)

and Unify : sig
  val (=?) : S.t -> S.t -> unit
end = struct

  (* syntactic unification *)
  let (=?) r = r |> unite ~sel:begin curry @@ function
    | _ -> failwith "todo"
  end

end
