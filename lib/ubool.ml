open! Batteries
open Either
open Uref

exception BUError

module type Constant = sig
  type t
  val and_const : t -> t -> t
  val xor_const : t -> t -> t
  val one : t
  val zero : t
  val to_string : t -> string
end

module Make(C : Constant) = struct
  
  type boolean = boolean_ ref
  and boolean_ = 
    | BExpr of boolean list list
    | BVar of int
    | BConst of C.t
  
  type ubool = boolean list list uref

  open Printf
  let rec pretty_ubool out = uget %> pretty_ubool_ out
  and pretty_ubool_ out = function
    | [] -> fprintf out "%s" @@ C.to_string C.zero
    | h :: t -> 
      pretty_ubool_inner out h;
      List.iter (fun x -> fprintf out " ^ "; pretty_ubool_inner out x) t
  and pretty_ubool_inner out = function
    | [] -> fprintf out "%s" @@ C.to_string C.one
    | h :: t -> 
      pretty_boolean out h;
      List.iter (fun x -> fprintf out " & "; pretty_boolean out x) t
  and pretty_boolean out = (!) %> pretty_boolean_ out
  and pretty_boolean_ out = function
    | BExpr e -> pretty_ubool_ out e
    | BVar i -> fprintf out "X%d" i
    | BConst c -> fprintf out "%s" @@ C.to_string c
  let pretty_print printer in_ = 
    let out = IO.output_string () in
    printer out in_;
    IO.close_out out
  
  type prop = 
    | PConst of C.t
    | PVar of int
    | PNot of prop
    | POr of prop list
    | PAnd of prop list
    | PXor of prop list
  
  let rec pretty_prop_paren out f paren = 
    match f with
      | PConst c -> fprintf out "%s" @@ C.to_string c
      | PVar i -> fprintf out "X%d" i
      | PNot e ->
        fprintf out "¬";
        pretty_prop_paren out e paren
      | POr e -> pretty_prop_list_paren out e " \\/ " paren
      | PAnd e -> pretty_prop_list_paren out e " /\\ " paren
      | PXor e -> pretty_prop_list_paren out e " ^ " paren
  and pretty_prop_list_paren out expr sep paren = 
    if paren then fprintf out "(";
    pretty_prop_list out expr sep;
    if paren then fprintf out ")"
  and pretty_prop_list out expr sep = match expr with
    | [] -> ()
    | [e] -> pretty_prop_paren out e true
    | e :: rest ->
      pretty_prop_paren out e true;
      fprintf out "%s" sep;
      pretty_prop_list out rest sep
  let pretty_prop out f = pretty_prop_paren out f false

  let rec anf_to_prop expr = match expr with
    | BExpr e -> PXor (List.map (fun a -> PAnd (List.map (fun b -> anf_to_prop !b) a)) e)
    | BVar i -> PVar i
    | BConst c -> PConst c
  
  let rec anf_no_xor expr = match expr with
    | PXor [e] -> e
    | PXor [e1; e2] -> POr [PAnd [e1; PNot e2]; PAnd [PNot e1; e2]]
    | PXor (e1 :: e2 :: rest) -> anf_no_xor (PXor (POr [PAnd [e1; PNot e2]; PAnd [PNot e1; e2]] :: rest))
    | _ -> failwith "Wrong propositional formula format."
  
  let rec anf_no_not expr = match expr with
    | PNot (PConst e) -> PConst (if e = C.one then C.zero else C.one)
    | PNot (PNot e) -> anf_no_not e
    | PNot (POr e1) -> PAnd (List.map (fun e2 -> anf_no_not (PNot e2)) e1)
    | PNot (PAnd e1) -> POr (List.map (fun e2 -> anf_no_not (PNot e2)) e1)
    | POr e1 -> POr (List.map (fun e2 -> anf_no_not e2) e1)
    | PAnd e1 -> PAnd (List.map (fun e2 -> anf_no_not e2) e1)
    | e -> e

  let findb f l = match List.find_opt f l with
    | Some _ -> true
    | None -> false
  let rec anf_no_const expr = let f = List.map (fun e -> anf_no_const e) in match expr with
    | POr e1 ->
      let e2 = f e1 in
      let q = findb (fun e3 -> match e3 with PConst c -> c = C.one | _ -> false) e2 in
      if q then
        PConst C.one
      else
        let res = List.filter (fun e3 -> match e3 with PConst c -> c <> C.zero | _ -> true) e2 in
        if List.is_empty res then PConst C.zero else POr res
    | PAnd e1 ->
      let e2 = f e1 in
      let q = findb (fun e3 -> match e3 with PConst c -> c = C.zero | _ -> false) e2 in
      if q then
        PConst C.zero
      else
        let res = List.filter (fun e3 -> match e3 with PConst c -> c <> C.one | _ -> true) e2 in
        if List.is_empty res then PConst C.one else PAnd res
    | e -> e
  
  let rec anf_flatten expr = let f = List.map (fun e -> anf_flatten e) in match expr with
    | POr [e] -> anf_flatten e
    | POr e1 ->
      let e2 = f e1 in
      let e_or = List.filter (fun e3 -> match e3 with POr _ -> true | _ -> false) e2 in
      let e_not_or = List.filter (fun e3 -> match e3 with POr _ -> false | PAnd [] -> false | _ -> true) e2 in
      POr (List.fold (fun a e3 -> match e3 with POr e4 -> List.append (f e4) a | _ -> assert false) (f e_not_or) e_or)
    | PAnd [e] -> anf_flatten e
    | PAnd e1 ->
      let e2 = f e1 in
      let e_and = List.filter (fun e3 -> match e3 with PAnd _ -> true | _ -> false) e2 in
      let e_not_and = List.filter (fun e3 -> match e3 with PAnd _ -> false | POr [] -> false | _ -> true) e2 in
      PAnd (List.fold (fun a e3 -> match e3 with PAnd e4 -> List.append (f e4) a | _ -> assert false) (f e_not_and) e_and)
    | e -> e
  
  let rec anf_distribute expr = let f = List.map (fun e -> anf_distribute e) in match expr with
    | POr e1 ->
      let e2 = f e1 in
      anf_flatten (POr e2)
    | PAnd e1 ->
      let e2 = f e1 in
      anf_flatten (POr (anf_distribute_or e2))
    | e -> e
  and anf_distribute_or expr = match expr with
    | [] -> []
    | [e] -> [e]
    | e1 :: e2 :: rest -> anf_distribute_or ((anf_distribute_single e1 e2) :: rest)
  and anf_distribute_single e1 e2 = match e1, e2 with
    | POr (e3 :: rest), POr e4 -> POr [anf_distribute_single e3 (POr e4); anf_distribute_single (POr rest) (POr e4)]
    | POr e3, POr (e4 :: rest) -> POr [anf_distribute_single (POr e3) e4; anf_distribute_single (POr e3) (POr rest)]
    | POr (e3 :: rest), e4 -> POr [PAnd [e3; e4]; anf_distribute_single (POr rest) e4]
    | POr [], _ -> POr []
    | e3, POr (e4 :: rest) -> POr [PAnd [e3; e4]; anf_distribute_single e3 (POr rest)]
    | _, POr [] -> POr []
    | e3, e4 -> anf_distribute_single (POr [e3]) (POr [e4])
  
  let anf_to_dnf expr = anf_distribute (anf_flatten (anf_no_const (anf_no_not (anf_no_xor (anf_to_prop expr)))))
  
  let bfresh_ () = [[ref (BVar (unique ()))]]
  let bfresh () = uref (bfresh_ ())
  
  let mul_basic b1 b2 = 
    List.concat_map (fun p1 -> List.map ((@) p1) b2) b1
  
  let coal_mul bs = 
    match bs |> List.partition_map @@ fun x -> match !x with
      | BConst c -> Right c
      | _ -> Left x with
    | vs, [] -> vs
    | vs, cs -> 
      let c' = List.fold C.and_const C.one cs in
      if c' = C.zero then [ref (BConst (C.zero))]
      else List.sort_unique Stdlib.compare @@
        if c' = C.one then vs
        else ref (BConst c') :: vs

  let coal_add b = 
    match b |> List.partition_map @@ fun x -> match List.map (!) x with
      | [BConst c] -> Right c
      | _ -> Left x with
    | vs, [] -> vs
    | vs, cs -> [ref (BConst (List.fold C.xor_const C.zero cs))] :: vs

  let mul_idem b1 b2 = 
    mul_basic b1 b2 |> List.map @@ List.sort_uniq Stdlib.compare %> coal_mul

  let cancel_dups = 
    let[@tail_mod_cons] rec go = function
      | h1 :: h2 :: t when h1 = h2 -> go t
      | h :: t -> h :: go t
      | [] -> [] in
    List.sort Stdlib.compare %> go

  let add_xor b1 b2 = b1 @ b2 |> cancel_dups |> coal_add
  
  let rec simp b = 
    List.map begin fun bs -> 
      match List.partition_map begin fun x -> match !x with
        | BExpr e -> Right (simp e)
        | _ -> Left x
      end bs with
      | xs, [] -> [xs]
      | xs, es -> List.fold mul_idem [xs] es
    end b
    |> List.flatten |> cancel_dups |> coal_add
  
  let tally m v = 
    m |> Hashtbl.modify_opt v @@ function
      | Some i -> Some (i + 1)
      | None -> Some 1
  
  let get_var b = 
    let m = Hashtbl.create 16 in
    b |> List.iter (List.iter ((!) %> function
      | BVar i -> tally m i
      | _ -> ()));
    Hashtbl.bindings m
    |> List.sort (fun x y -> Stdlib.compare (snd y) (snd x))
    |> List.map fst |> function
      | h :: _ -> h  (* variable with *most* counts *)
      | [] -> raise BUError  (* no variables *)
  
  let extract b = 
    let v = get_var b in
    let v_ref = List.flatten b |> List.find ((!) %> (=) (BVar v)) in
    let has_v, no_v = List.partition (List.map (!) %> List.mem (BVar v)) b in
    let t1 = List.map (List.filter ((!) %> (<>) (BVar v))) has_v in
    v_ref, t1, no_v
  
  let rec solve t0 = 
    (* print_endline @@ "solving: " ^ (pretty_print pretty_ubool_ t0); *)
    let t = simp t0 in
    match t with
    | [] -> ()
    | [[x]] when !x = BConst C.zero -> ()
    | _ -> 
        let x, t1, t2 = extract t in
        let t1c = add_xor t1 [[]] in
        solve (mul_idem t1c t2);
        let sol = simp (add_xor (mul_idem t1c (bfresh_ ())) t2) in
        x := BExpr sol
  
  let size = List.flatten %> List.length
  let smaller x y = if size y > size x then y else x
  let unify = unite ~sel:begin fun x y -> 
    solve (x @ y); 
    smaller (simp x) (simp y)
  end

end
