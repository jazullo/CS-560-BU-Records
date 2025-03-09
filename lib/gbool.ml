(* B-generalization *)
open! Batteries

module type Poly = sig
  include Ubool.Constant
  val bmatch : t -> t -> t * t
  val factor_consts : t -> t list * t
  val vars : t -> t list
  val replace : t -> t -> t -> t
  val project : t list -> t -> t
  val minlvl : t -> t -> t
  val simplify : t -> unit
  val inter_consts : t list -> t list -> t list
  val diff_consts : t list -> t list -> t list
end

module Make(B : Poly) = struct

  let eq x y = B.(is_zero (add x y))
  let mem x = List.exists (eq x)
  let inter (c1, l1) (c2, l2) = 
    B.inter_consts c1 c2, List.filter (fun x -> mem x l2) l1
  let diff (c1, l1) (c2, l2) = 
    B.diff_consts c1 c2, List.filter (not % fun x -> mem x l2) l1
  
  let product = uncurry (@) %> List.fold_left B.mul B.one

  (* Formal derivative over a Boolean polynomial *)
  let d f x = B.(add (replace x zero f) (replace x one f))

  (* Factorization core based on formal derivatives *)
  (* "On a Polytime Factorization Algorithm from Multilinear Polynomials..." *)
  let fd f = match B.vars f with
    | [] -> None
    | x :: t -> 
      let g = B.mul B.(replace x zero f) (d f x) in
      let same, other = List.fold_left (fun (same, other) y -> 
          if B.is_zero (d g y) then same, y :: other
          else y :: same, other
        ) ([x], []) t in
      match other with
      | [] -> None
      | _ :: _ -> Some B.(project same f, project other f)

  let factorize t =  (* Bot = None because of infinite factorization *)
    let tc, tf = B.factor_consts t in
    List.iter B.simplify tc;
    tc, List.unfold tf fd
  
  let gen dense sparse arr = 
    let rec go1 d1 s1 = match d1 with
      | [] -> () | v1 :: d2 -> 
        List.iter (fun v2 -> 
          let p3, p4, p5, v = B.(ref zero, ref zero, ref zero, minlvl v1 v2) in
          let skip = ref false in
          let a = Array.mapi (fun i r -> 
            if !skip then arr.(i) else
              let t1, t2 = B.bmatch v1 r in
              let t3, t4 = B.bmatch v2 t1 in
              let t5, t6 = B.bmatch v2 t2 in
              let t3_fac, t4_fac, t5_fac = 
                factorize t3, factorize t4, factorize t5 in
              let gcd = inter t3_fac (inter t4_fac t5_fac) in
              let t3' = product (diff t3_fac gcd) in
              let t4' = product (diff t4_fac gcd) in
              let t5' = product (diff t5_fac gcd) in
              if B.(is_zero t3 && is_zero t4 && is_zero t5) then arr.(i)
              else if B.(is_zero !p3 && is_zero !p4 && is_zero !p5)
              then (p3 := t3'; p4 := t4'; p5 := t5'; B.(add (mul (product gcd) v) t6))
              else if eq t3' !p3 && eq t4' !p4 && eq t5' !p5
              then B.(add (mul (product gcd) v) t6)
              else (skip := true; arr.(i))
          ) arr in
        if !skip || B.(is_zero !p3 && is_zero !p4 && is_zero !p5) then ()
        else Array.blit a 0 arr 0 (Array.length a)) (d2 @ s1);
        go1 d2 s1 in
    go1 dense sparse

end
