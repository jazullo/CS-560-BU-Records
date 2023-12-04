open! Batteries

(* straight ANF to DNF conversion *)
(* xor can be thought of as a parity operator *)
(* idea is that we can guess groups of 2 at a time *)

let ( let* ) x f = Set.fold (f %> Set.union) x Set.empty
let ( let+ ) x f = Set.map f x

let guess anf = 
  let+ x = anf in
  Set.singleton x, Set.remove x anf

let guess2 anf = 
  let* x, xs = guess anf in
  let+ y, ys = guess xs in
  Set.union x y, ys

let guess_partitions anf = 
  let* x, xs = guess anf in
  let rec go l r = 
    Set.add (l, r) @@
      if Set.cardinal r < 2 then Set.empty
      else
        let* g, r' = guess2 r in
        go (Set.union l g) r' in
  go x xs

let dnf negate = 
  guess_partitions
  %> Set.map (fun (l, r) -> Set.union l (negate r))
