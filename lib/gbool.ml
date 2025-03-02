(* B-generalization *)
open! Batteries

module type Poly = sig
  include Ubool.Constant
  val bmatch : int -> t -> t * t
  val uvar : int -> t
end

module Make(B : Poly) = struct

  let (<|>) x y = B.(add (add x y) (mul x y))

  let eq x y = B.(is_zero (add x y))

  (* let intersymdiff l1 l2 = 
    let l_codiff, l_i, l_diff = List.fold_left (fun (l2', inter, symdiff) x -> 
      if List.mem_cmp (fun a b -> if eq a b then 0 else compare a b) x l2'
      then List.remove l2' x, x :: inter, symdiff
      else l2', inter, x :: symdiff) (l2, [], []) l1 in
    l_i, l_codiff, l_diff *)
  
  let inter l1 l2 = List.filter (fun x -> 
    List.mem_cmp (fun a b -> if eq a b then 0 else compare a b) x l2) l1
  
  let diff l1 l2 = List.filter (not % fun x -> 
    List.mem_cmp (fun a b -> if eq a b then 0 else compare a b) x l2) l1
  
  let product = List.fold_left B.mul B.one

  let factorize _ = failwith "todo"

  let gen vars arr = 
    let rec go = function
      | [] -> () | v1 :: vs -> 
        List.iter (fun v2 -> 
          let p3, p4, p5 = B.(ref zero, ref zero, ref zero) in
          let skip = ref false in
          let a = Array.mapi (fun i r -> 
            if !skip then arr.(i) else
              let t1, t2 = B.bmatch v1 r in
              let t3, t4 = B.bmatch v2 t1 in
              let t5, t6 = B.bmatch v2 t2 in
              let t = t3 <|> t4 <|> t5 in
              let t3_fac, t4_fac, t5_fac = 
                factorize t3, factorize t4, factorize t5 in
              let gcd = inter t3_fac (inter t4_fac t5_fac) in
              let t3' = product (diff t3_fac gcd) in
              let t4' = product (diff t4_fac gcd) in
              let t5' = product (diff t5_fac gcd) in
              if B.(is_zero t3 && is_zero t4 && is_zero t5) then arr.(i)
              else if B.(is_zero !p3 && is_zero !p4 && is_zero !p5)
              then (p3 := t3'; p4 := t4'; p5 := t5'; B.(add (mul t (uvar v1)) t6))
              else if eq t3' !p3 && eq t4' !p4 && eq t5' !p5
              then B.(add (mul t (uvar v1)) t6)
              else (skip := true; arr.(i))
          ) arr in
        if !skip || B.(is_zero !p3 && is_zero !p4 && is_zero !p5) then ()
        else Array.blit a 0 arr 0 (Array.length a)) vs in
    go vars

end
