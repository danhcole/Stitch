%{ open Stch_ast %}

%token SEMI SQUOTE DQUOTE LPAREN RPAREN LSQUARE RSQUARE LBRACE RBRACE
%token COMMA TIMES DIVIDE ADD SUBTRACT MOD
%token ACCESS ASSIGN EQUAL NEGATE NE
%token AND OR
%token GT LE LT LE
%token FROM TO BY
%token IF ELSE WHILE FOR STITCH BREAK RETURN TVOIDT TINTT TFLOATT TCHART TARRAYT TSTRUCTT
%token CONST VOID
%token <int>INT
%token <char>CHAR
%token <float> FLOAT 
%token <string> ID
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left EQUAL NE
%left LT GT LE GE
%left ADD SUBTRACT
%left TIMES DIVIDE MOD
%right BNOT NEGATE
%left ACCESS

%start program
%type <stch_ast.program> program

%%

program:
	decls EOF {$1}

decls:
/*nothing*/ { [], [] }
| decls vdecl { ($2 :: fst $1), snd $1}
| decls fdecl { fst $1, ($2 :: snd $1) }

fdecl:
	type_name ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE 
	{ { ftype = $1;
		fname = $2;
		formals = $4;
		locals = List.rev $7;
		body = List.rev $8; } }

type_name:
TINTT			{ "int" }
| TFLOATT		{ "float" }
| TCHART 		{ "char" }
| TINTT TARRAYT  	{ "int array" }
| TFLOATT TARRAYT	{ "float array" }
| TCHART TARRAYT	{ "char array" }

array_opt:
	/* nothing */			{ -1 }
	| LSQUARE INT RSQUARE	{ $2 }
	| LSQUARE RSQUARE		{ -1 }

formals_opt:
	/* nothing */	{ [] }
| formal_list		{ List.rev $1 }

formal_list:
  ID					{ [$1] }
| formal_list COMMA ID	{ $3 :: $1 }

vdecl_list:
/*nothing*/			{ [] }
| vdecl_list vdecl	{$2 ::$1}

vdecl:
	type_name ID array_opt SEMI
	{{
		vdecl_type 	= $1;
		vdecl_name	= $2;
		array_size	= $3;
	}}

stmt_list:
/*nothing*/			{ [] }
| stmt_list stmt	{ $2 :: $1 }

stmt:
	expr SEMI	{ Expr($1) }
| RETURN expr SEMI { Return($2) }
| LBRACE stmt_list RBRACE { Block(List.rev $2) }
| IF LPAREN expr RPAREN stmt %prec NOELSE	{ IF($3, $5, Block([])) }
| IF LPAREN expr RPAREN stmt ELSE stmt		{ IF($3, $5, $7) }
| FOR LPAREN expr_opt SEMI expr_opt SEMI expr_opt RPAREN stmt
	{ For($3,$5,$7,$9) }
| WHILE LPAREN expr RPAREN stmt				{While($3, $5) }
| STITCH expr FROM expr TO expr BY expr stmt	{Stitch($2,$4,$6,$8,$9)}
| BREAK SEMI	{Break}


expr_opt:
/*nothing*/		{ Noexpr }
| expr			{ $1 }

expr:
/*Primitives*/
INT			{ Int($1) }
| FLOAT		{ Float($1) }
| CHAR		{ Char($1) }
| ID		{ Id($1) }
/*Array*/
| LSQUARE array_list_opt RSQUARE { $2 }
/*Arithmetic*/
| expr ADD expr			{ Binop($1, Add,   $3) }
| expr SUBTRACT  expr	{ Binop($1, Subtract,   $3) }
| expr TIMES  expr		{ Binop($1, Times,  $3) }
| expr DIVIDE expr		{ Binop($1, Divide,   $3) }
| expr MOD expr			{ Binop($1, Mod, $3) }
/*Comparison*/
| expr EQUAL expr		{ Binop($1, Equal, $3) }
| expr NE    expr		{ Binop($1, Ne,   $3) }
| expr LT     expr		{ Binop($1, Lt,  $3) }
| expr LE    expr		{ Binop($1, Le,   $3) }
| expr GT     expr		{ Binop($1, Gt,  $3) }
| expr GE  expr			{ Binop($1, Ge,   $3) }
/*Logical*/
| expr OR expr			{ Binop($1, Or, $3) }
| expr AND expr			{ Binop($1, And, $3) }
/*Unary*/
| NEGATE expr			{ Negate($2)}
/*Miscellanenous*/
| ID ACCESS ID			{ Access($1, $3) }
| ID ASSIGN expr		{ Assign($1, $3) }
/*| ID LPAREN actuals_opt RPAREN { Call($1, $3) }*/
| LPAREN expr RPAREN	{ $2 } 

array_list_opt:
/*nothing*/		{ [] }
| array_list	{ List.rev $1 }

array_list:
expr			{ [$1] }
|array_list COMMA expr	{$3 :: $1 }
