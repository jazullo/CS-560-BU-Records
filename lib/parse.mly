%{
  [@@@warning "-5"]  (* let _ = _menhir_action_40 () in  ??? *)
  open! Batteries
  open! Uref
  open! Ast

  let fresh () = uref (Types.S.MVar (!Common.level, unique ()))
%}

%token EOF
%token LPAREN RPAREN LBRACE RBRACE COMMA PERIOD FUN ARROW
%token ADD SUB MUL DIV MOD NOT OR AND CONCAT INTERSECT UPDATE EQ NE LT LE GT GE
%token DEF LET IN IF THEN ELSE
%token TRUE FALSE
%token<int> LIT
%token<string> ID

%start<def list> program_file

%nonassoc ELSE
%right IN
%right ARROW
%nonassoc NOT EQ NE LT LE GT GE
%left ADD SUB
%left MUL DIV MOD
%left INTERSECT
%left UPDATE
%left CONCAT
%left AND
%left OR
%left PERIOD

%%

program_file: program EOF {$1}

program:
  | def program {$1 :: $2}
  | {[]}

def: DEF ID pat_seq EQ expr {(($2, ($3) ($5)), $loc, fresh ())}

expr: 
  | IF expr THEN expr ELSE expr {(Ternary ($2, $4, $6), $loc, fresh ())}
  | expr ADD expr {(Arithmetic ($1, Add, $3), $loc, fresh ())}
  | expr SUB expr {(Arithmetic ($1, Sub, $3), $loc, fresh ())}
  | expr MUL expr {(Arithmetic ($1, Mul, $3), $loc, fresh ())}
  | expr DIV expr {(Arithmetic ($1, Div, $3), $loc, fresh ())}
  | expr MOD expr {(Arithmetic ($1, Mod, $3), $loc, fresh ())}
  | expr OR expr {(Logical ($1, Or, $3), $loc, fresh ())}
  | expr AND expr {(Logical ($1, And, $3), $loc, fresh ())}
  | expr EQ expr {(Comparative ($1, Eq, $3), $loc, fresh ())}
  | expr NE expr {(Comparative ($1, Ne, $3), $loc, fresh ())}
  | expr LT expr {(Comparative ($1, Lt, $3), $loc, fresh ())}
  | expr LE expr {(Comparative ($1, Le, $3), $loc, fresh ())}
  | expr GT expr {(Comparative ($1, Gt, $3), $loc, fresh ())}
  | expr GE expr {(Comparative ($1, Ge, $3), $loc, fresh ())}
  | expr CONCAT expr {(Record ($1, Concatenate, $3), $loc, fresh ())}
  | expr INTERSECT expr {(Record ($1, Intersect, $3), $loc, fresh ())}
  | expr UPDATE expr {(Record ($1, Update, $3), $loc, fresh ())}
  | LET ID pat_seq EQ expr IN expr {(Binding ($2, ($3) ($5), $7), $loc, fresh ())}
  | FUN pat_seq ARROW expr {($2) ($4)}
  | expr2 {$1}

expr2: 
  | expr2 expr3 {(Apply ($1, $2), $loc, fresh ())}
  | expr3 {$1}

expr3:
  | NOT expr3 {(Not $2, $loc, fresh ())}
  | expr3 PERIOD ID {(Project ($1, $3), $loc, fresh ())}
  | LPAREN expr RPAREN {$2}
  | LBRACE separated_list(COMMA, separated_pair(ID, EQ, expr)) RBRACE {(RecordCon $2, $loc, fresh ())}
  | LIT {(IntLit $1, $loc, fresh ())}
  | TRUE {(BoolLit true, $loc, fresh ())}
  | FALSE {(BoolLit false, $loc, fresh ())}
  | ID {(Id $1, $loc, fresh ())}

pat: 
  | ID {Param $1, $loc, fresh ()}
  | pat CONCAT pat {CatPat ($1, $3), $loc, fresh ()}
  | LBRACE separated_list(COMMA, separated_pair(ID, EQ, pat)) RBRACE {RecPat $2, $loc, fresh ()}
  | LPAREN pat RPAREN {$2}

pat_seq: 
  | pat pat_seq {fun x -> Abstract ($1, ($2) x), $loc, fresh ()}
  | {fun x -> x}
