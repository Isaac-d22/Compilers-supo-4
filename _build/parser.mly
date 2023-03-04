/* File parser.mly */
// Convert tokens to abstract syntax tree
%{

let get_loc = Parsing.symbol_start_pos 

%}

%token <int> INT
%token <bool> BOOL
%token <int> LOCATION
%token ADD GTE SEMICOLON 
%token SKIP DEREF ASSIGN LOCATION
%token IF THEN ELSE WHILE DO
%token LPAREN RPAREN
%token BEGIN END
%token EOF
%left ADD


%start main
%type <Past.expr> simple_expr 
%type <Past.expr> expr 
%type <Past.expr list> exprlist
%type <Past.expr> main

%%
main:
	expr EOF                { $1 }
;
simple_expr:
| INT                                { Past.Integer (get_loc(), $1) }
| BOOL                               { Past.Bool (get_loc(), $1) }
| LPAREN expr RPAREN                 { $2 }

expr:
| simple_expr                        {  $1 }
| expr ADD expr                      { Past.Op(get_loc(), $1, Past.ADD, $3) }
| expr GTE expr                      { Past.Op(get_loc(), $1, Past.GTE, $3) }
| DEREF INT                     { Past.Deref(get_loc(), $2) }
| INT ASSIGN expr				 { Past.Assign(get_loc(), $1, $3) }
| IF expr THEN expr ELSE expr        { Past.If(get_loc(), $2, $4, $6) }
| WHILE expr DO expr			     { Past.While(get_loc(), $2, $4) }
| BEGIN exprlist END                 { Past.Seq(get_loc(), $2) }

exprlist:
|   expr                             { [$1] }
|   expr  SEMICOLON exprlist         { $1 :: $3  }


