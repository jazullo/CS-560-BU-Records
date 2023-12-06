%{
  open! Batteries
  open! Uref
  open! Ast
%}

%token EOF
%token LPAREN RPAREN LBRACE RBRACE COMMA PERIOD BACKSLASH ARROW
%token ADD SUB MUL DIV MOD NOT OR AND CONCAT INTERSECT EQ NE LT LE GT GE
%token DEF LET IN IF THEN ELSE END
%token TRUE FALSE
%token<int> LIT
%token<string> ID

%start<def list> program_file

%left ADD SUB OR AND
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
  | IF expr THEN expr ELSE expr END {(Ternary ($2, $4, $6), $loc, uref (Types.S.MVar 0))}
  | expr LPAREN expr_list RPAREN {(Apply ($1, $3), $loc, uref (Types.S.MVar 0))}
  | expr ADD expr {(Arithmetic ($1, Add, $3), $loc, uref (Types.S.MVar 0))}
  | expr SUB expr {(Arithmetic ($1, Sub, $3), $loc, uref (Types.S.MVar 0))}
  | expr MUL expr {(Arithmetic ($1, Mul, $3), $loc, uref (Types.S.MVar 0))}
  | expr DIV expr {(Arithmetic ($1, Div, $3), $loc, uref (Types.S.MVar 0))}
  | expr MOD expr {(Arithmetic ($1, Mod, $3), $loc, uref (Types.S.MVar 0))}
  | NOT expr {(LogicalUnary (Not, $2), $loc, uref (Types.S.MVar 0))}
  | expr OR expr {(Logical ($1, Or, $3), $loc, uref (Types.S.MVar 0))}
  | expr AND expr {(Logical ($1, And, $3), $loc, uref (Types.S.MVar 0))}
  | expr EQ expr {(Comparative ($1, Eq, $3), $loc, uref (Types.S.MVar 0))}
  | expr NE expr {(Comparative ($1, Ne, $3), $loc, uref (Types.S.MVar 0))}
  | expr LT expr {(Comparative ($1, Lt, $3), $loc, uref (Types.S.MVar 0))}
  | expr LE expr {(Comparative ($1, Le, $3), $loc, uref (Types.S.MVar 0))}
  | expr GT expr {(Comparative ($1, Gt, $3), $loc, uref (Types.S.MVar 0))}
  | expr GE expr {(Comparative ($1, Ge, $3), $loc, uref (Types.S.MVar 0))}
  | expr CONCAT expr {(Record ($1, Concatenate, $3), $loc, uref (Types.S.MVar 0))}
  | expr INTERSECT expr {(Record ($1, Intersect, $3), $loc, uref (Types.S.MVar 0))}
  | LPAREN expr RPAREN {$2}
  | LBRACE asgn_list RBRACE {(RecordCon $2, $loc, uref (Types.S.MVar 0))}
  | expr PERIOD ID {(Project ($1, $3), $loc, uref (Types.S.MVar 0))}
  | LET asgn IN expr {(let (x, y) = $2 in Binding (x, y, $4), $loc, uref (Types.S.MVar 0))}
  | BACKSLASH ID ID* ARROW expr {(Abstract ($2 :: $3, $5), $loc, uref (Types.S.MVar 0))}
  | LIT {(IntLit $1, $loc, uref (Types.S.MVar 0))}
  | TRUE {(BoolLit true, $loc, uref (Types.S.MVar 0))}
  | FALSE {(BoolLit false, $loc, uref (Types.S.MVar 0))}

expr_list:
  | {[]}
  | expr {[$1]}
  | expr COMMA expr_list {$1 :: $3}

asgn: ID EQ expr {($1, $3)}

asgn_list:
  | {[]}
  | asgn {[$1]}
  | asgn COMMA asgn_list {$1 :: $3}
