%{
  open! Batteries
  module Brml = struct  (* evil dependency hack *)
    module Types = Types
  end
  let c = ref (-1)
  let unique () = incr c; !c
  let fresh () = Uref.uref (Types.S.MVar (0, unique ()))
  let bfresh () = Uref.uref (Types.Free.Var (0, unique ()))
  let ht = Hashtbl.create 32
  let intern x = Hashtbl.find_option ht x |> Option.default_delayed @@ fun () -> 
    let nu = fresh () in
    Hashtbl.add ht x nu; nu
  let ht_row = Hashtbl.create 32
  let intern_row x = Hashtbl.find_option ht_row x |> Option.default_delayed @@ fun () -> 
    let nu = bfresh () in
    Hashtbl.add ht_row x nu; nu
  let then_clear x = Hashtbl.(clear ht; clear ht_row); c := (-1); x
%}


%token EOF LPAREN RPAREN LBRACE RBRACE LANGLE RANGLE
%token ARROW ADD OR COMMA SEMICOLON INT BOOL COLON BANG
%token<string> ID AID BID
%left ADD OR
%start<Types.Free.t list * Types.Free.t list> system
%%

system: 
  | separated_pair(separated_list(COMMA, rowtype), SEMICOLON, list(BID)) EOF
    {then_clear (Tuple2.map2 (List.map (Hashtbl.find ht_row)) $1)}
  | separated_list(COMMA, rowtype) EOF {then_clear ($1, List.of_enum (Hashtbl.values ht_row))}

mltype: 
  | mltype1 ARROW mltype {Uref.uref (Types.S.MFun ($1, $3))}
  | mltype1 {$1}

%inline mltype1: 
  | INT {Uref.uref Types.S.(MLit MInt)}
  | BOOL {Uref.uref Types.S.(MLit MBool)}
  | LPAREN mltype RPAREN {$2}
  | LANGLE rowtype RANGLE {Uref.uref (Types.S.TRec $2)}
  | AID {intern $1}

rowtype: 
  | rowtype ADD rowtype {Types.Free.add_t $1 $3}
  | rowtype OR rowtype {Types.Free.(add_t (add_t $1 $3) (mul_t $1 $3))}
  | nonempty_list(rowtype1)
      { let[@warning "-8"] (h :: t) = $1 in
        List.fold_left Types.Free.mul_t h t }

(* must be factored out due to menhir bug *)
%inline disjoin(a, b, c): a {$1} | b {$1} | c {$1}
%inline rowtype1: 
  | boption(BANG) LBRACE separated_nonempty_list(COMMA, 
    separated_pair(disjoin(ID, AID, BID), COLON, mltype)) RBRACE
    {Types.(Free.uconst ((if $1 then Inv else Fin), Dict.of_list $3))}
  | LPAREN rowtype RPAREN {$2}
  | BID {intern_row $1}
