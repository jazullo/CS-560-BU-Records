%{
  open! Batteries
  open! Uref
  open! Ast

  let fresh () = uref (Types.S.MVar (!Common.level, unique ()))
%}

%token EOF
%token LPAREN RPAREN LBRACE RBRACE COMMA PERIOD FUN ARROW
%token ADD SUB MUL DIV MOD NOT OR AND CONCAT INTERSECT EQ NE LT LE GT GE
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
%left CONCAT
%left AND
%left OR
%left PERIOD

%%

program_file: program EOF {$1}

program:
  | def program {$1 :: $2}
  | {[]}

def: DEF ID pat* EQ expr {(($2, $3, $5), $loc)}

expr:
  | NOT expr {(LogicalUnary (Not, $2), $loc, fresh ())}
  | expr PERIOD ID {(Project ($1, $3), $loc, fresh ())}
  | expr2 {$1}

expr2: 
  | expr3 expr3+ {(Apply ($1, $2), $loc, fresh ())}
  | expr3 {$1}

expr3: 
  | IF expr THEN expr ELSE expr3 {(Ternary ($2, $4, $6), $loc, fresh ())}
  | expr3 ADD expr3 {(Arithmetic ($1, Add, $3), $loc, fresh ())}
  | expr3 SUB expr3 {(Arithmetic ($1, Sub, $3), $loc, fresh ())}
  | expr3 MUL expr3 {(Arithmetic ($1, Mul, $3), $loc, fresh ())}
  | expr3 DIV expr3 {(Arithmetic ($1, Div, $3), $loc, fresh ())}
  | expr3 MOD expr3 {(Arithmetic ($1, Mod, $3), $loc, fresh ())}
  | expr3 OR expr3 {(Logical ($1, Or, $3), $loc, fresh ())}
  | expr3 AND expr3 {(Logical ($1, And, $3), $loc, fresh ())}
  | expr3 EQ expr3 {(Comparative ($1, Eq, $3), $loc, fresh ())}
  | expr3 NE expr3 {(Comparative ($1, Ne, $3), $loc, fresh ())}
  | expr3 LT expr3 {(Comparative ($1, Lt, $3), $loc, fresh ())}
  | expr3 LE expr3 {(Comparative ($1, Le, $3), $loc, fresh ())}
  | expr3 GT expr3 {(Comparative ($1, Gt, $3), $loc, fresh ())}
  | expr3 GE expr3 {(Comparative ($1, Ge, $3), $loc, fresh ())}
  | expr3 CONCAT expr3 {(Record ($1, Concatenate, $3), $loc, fresh ())}
  | expr3 INTERSECT expr3 {(Record ($1, Intersect, $3), $loc, fresh ())}
  | LPAREN expr RPAREN {$2}
  | LBRACE separated_list(COMMA, separated_pair(ID, EQ, expr)) RBRACE {(RecordCon $2, $loc, fresh ())}
  | LET ID pat* EQ expr IN expr3 {(Binding ($2, $3, $5, $7), $loc, fresh ())}
  | FUN pat* ARROW expr3 {(Abstract ($2, $4), $loc, fresh ())}
  | LIT {(IntLit $1, $loc, fresh ())}
  | TRUE {(BoolLit true, $loc, fresh ())}
  | FALSE {(BoolLit false, $loc, fresh ())}
  | ID {(Id $1, $loc, fresh ())}

pat: 
  | ID {Param $1, $loc}
  | LIT {IntPat $1, $loc}
  | TRUE {(BoolPat true, $loc)}
  | FALSE {(BoolPat false, $loc)}
  | pat CONCAT pat {CatPat ($1, $3), $loc}
  | LBRACE separated_list(COMMA, separated_pair(ID, EQ, pat)) RBRACE {RecPat $2, $loc}
  | LPAREN pat RPAREN {$2}
