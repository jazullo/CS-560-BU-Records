%{
  open! Batteries
  open! Uref
  open! Ast

  let fresh () = uref (Types.S.MVar (unique ()))
%}

%token EOF
%token LPAREN RPAREN LBRACE RBRACE COMMA PERIOD BACKSLASH ARROW
%token ADD SUB MUL DIV MOD NOT OR AND CONCAT INTERSECT EQ NE LT LE GT GE
%token DEF LET IN IF THEN ELSE END
%token TRUE FALSE
%token<int> LIT
%token<string> ID

%start<def list> program_file

%left ADD SUB
%left MUL DIV MOD
%left INTERSECT
%left CONCAT
%left AND
%left OR
%left PERIOD
%right IN ARROW LPAREN
%nonassoc NOT EQ NE LT LE GT GE

%%

program_file: program EOF {$1}

program:
  | def program {$1 :: $2}
  | {[]}

def: DEF ID ID* EQ expr {(($2, $3, $5), $loc)}

expr:
  | IF expr THEN expr ELSE expr END {(Ternary ($2, $4, $6), $loc, fresh ())}
  | expr LPAREN expr_list RPAREN {(Apply ($1, $3), $loc, fresh ())}
  | expr ADD expr {(Arithmetic ($1, Add, $3), $loc, fresh ())}
  | expr SUB expr {(Arithmetic ($1, Sub, $3), $loc, fresh ())}
  | expr MUL expr {(Arithmetic ($1, Mul, $3), $loc, fresh ())}
  | expr DIV expr {(Arithmetic ($1, Div, $3), $loc, fresh ())}
  | expr MOD expr {(Arithmetic ($1, Mod, $3), $loc, fresh ())}
  | NOT expr {(LogicalUnary (Not, $2), $loc, fresh ())}
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
  | LPAREN expr RPAREN {$2}
  | LBRACE asgn_list RBRACE {(RecordCon $2, $loc, fresh ())}
  | expr PERIOD ID {(Project ($1, $3), $loc, fresh ())}
  | LET asgn IN expr {(let (x, y) = $2 in Binding (x, [], y, $4), $loc, fresh ())} (* TODO *)
  | BACKSLASH ID ID* ARROW expr {(Abstract ($2 :: $3, $5), $loc, fresh ())}
  | LIT {(IntLit $1, $loc, fresh ())}
  | TRUE {(BoolLit true, $loc, fresh ())}
  | FALSE {(BoolLit false, $loc, fresh ())}

expr_list:
  | {[]}
  | expr {[$1]}
  | expr COMMA expr_list {$1 :: $3}

asgn: ID EQ expr {($1, $3)}

asgn_list:
  | {[]}
  | asgn {[$1]}
  | asgn COMMA asgn_list {$1 :: $3}
