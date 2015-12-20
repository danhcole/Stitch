%{ open Stch_ast %}

%token SEMI SQUOTE DQUOTE COLON LPAREN RPAREN LSQUARE RSQUARE LBRACE RBRACE
%token COMMA TIMES DIVIDE ADD SUBTRACT MOD
%token ASSIGN EQUAL NEGATE NE
%token AND OR
%token GT GE LT LE
%token FROM TO BY
%token IF ELSE WHILE FOR STITCH BREAK RETURN TVOID TINT TFLOAT TCHAR TINTAP TINTAM TFLOATAP TFLOATAM TFILE
%token VOID
%token <int> INT
%token <char> CHAR
%token <float> FLOAT 
%token <string> STRING
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
%right NEGATE

%start program
%type <Stch_ast.program> program

%%

program:
	/*decls EOF {$1}*/		{ [], [] }
	| program stmt 	 SEMI	{ ($2 :: fst $1), snd $1 }
	| program fdecl 		{ fst $1, ($2 :: snd $1) }

fdecl:
	type_name ID LPAREN formals_opt RPAREN LBRACE stmt_list RBRACE 
	{ { fdecl_type = $1;
		fdecl_name = $2;
		fdecl_formals = $4;
		body = List.rev $7; } }

type_name:
TINT			{ Tint }
| TFLOAT		{ Tfloat }
| TCHAR 		{ Tchar }
| TVOID 		{ Tvoid }
| TINTAP		{ Tintap }
| TINTAM		{ Tintam }
| TFLOATAP		{ Tfloatap }
| TFLOATAM		{ Tfloatam }
| TFILE			{ Tfile }

formals_opt:
	/* nothing */	{ [] }
| formal_list		{ List.rev $1 }

formal_list:
  vdecl						{ [$1] }
| formal_list COMMA vdecl	{ $3 :: $1 }

vdecl:
	type_name ID
	{{
		vdecl_type 	= $1;
		vdecl_name	= $2;
	}}

arraydecl:
	type_name ID LSQUARE expr_opt RSQUARE
	{{
		arraydecl_type = $1;
		arraydecl_name = $2;
		arraydecl_size = $4;
		}}

matrixdecl: /* two dimensional array implementation */
	type_name ID LSQUARE expr_opt RSQUARE LSQUARE expr_opt RSQUARE
	{{
		matrixdecl_type = $1;
		matrixdecl_name = $2;
		matrixdecl_rows = $4;
		matrixdecl_cols = $7;
		}}


stmt_list:
/*nothing*/			{ [] }
| stmt_list stmt	{ $2 :: $1 }

stmt:
  expr SEMI										{ Expr($1) }
| vdecl SEMI									{ Vdecl($1) }
/* One dimensional array stuff */
| arraydecl SEMI								{ ArrayDecl($1) }
| arraydecl ASSIGN LBRACE actuals_opt RBRACE SEMI
	{ ArrayInit($1, $4) }
/* Two dimensional array statements */
| matrixdecl SEMI								{ MatrixDecl($1) }
| matrixdecl ASSIGN LBRACE matrix_rev_list RBRACE SEMI
	{ MatrixInit($1, $4) }
| RETURN expr_opt SEMI 							{ Return($2) }
| LBRACE stmt_list RBRACE 						{ Block(List.rev $2) }
| IF LPAREN expr RPAREN stmt %prec NOELSE		{ If($3, $5, Block([])) }
| IF LPAREN expr RPAREN stmt ELSE stmt			{ If($3, $5, $7) }
| FOR LPAREN expr_opt SEMI expr_opt SEMI expr_opt RPAREN stmt
	{ For($3,$5,$7,$9) }
| WHILE LPAREN expr RPAREN stmt					{ While($3, $5) }
| STITCH expr FROM expr TO expr BY expr COLON stmt	
	{ Stitch($2,$4,$6,$8,$10) }
| vdecl ASSIGN expr SEMI						{ Assign($1, $3) }
| BREAK SEMI									{ Break }

expr_opt:
/*nothing*/		{ Noexpr }
| expr			{ $1 }

expr:
/*Primitives*/
| INT		{ Int($1) }
| FLOAT		{ Float($1) }
| CHAR		{ Char($1) }
| ID		{ Id($1) }
| STRING 	{ String($1) }
/*Array*/
| ID LSQUARE expr RSQUARE ASSIGN expr 		{ Array_Item_Assign($1, $3, $6) }
| ID LSQUARE expr RSQUARE					{ Array_Index_Access($1, $3) }
/*Matrix */
| ID LSQUARE expr RSQUARE LSQUARE expr RSQUARE ASSIGN expr
	{ Matrix_Item_Assign($1, $3, $6, $9) }

| ID LSQUARE expr RSQUARE LSQUARE expr RSQUARE
	{ Matrix_Index_Access($1, $3, $6) }
/*Arithmetic*/
| expr ADD 		expr	{ Binop($1, Add, $3) }
| expr SUBTRACT expr	{ Binop($1, Subtract, $3) }
| expr TIMES  	expr	{ Binop($1, Times, $3) }
| expr DIVIDE 	expr	{ Binop($1, Divide, $3) }
| expr MOD 		expr	{ Binop($1, Mod, $3) }
/*Comparison*/
| expr EQUAL expr		{ Binop($1, Equal, $3) }
| expr NE    expr		{ Binop($1, Ne, $3) }
| expr LT  	 expr		{ Binop($1, Lt, $3) }
| expr LE    expr		{ Binop($1, Le, $3) }
| expr GT    expr		{ Binop($1, Gt, $3) }
| expr GE    expr		{ Binop($1, Ge, $3) }
/*Logical*/
| expr OR expr			{ Binop($1, Or, $3) }
| expr AND expr			{ Binop($1, And, $3) }
/*Unary*/
| NEGATE expr			{ Negate($2)}
/*Miscellanenous*/
| ID LPAREN actuals_opt RPAREN { Call($1, $3) }
| LPAREN expr RPAREN	{ $2 } 
| ID ASSIGN expr		{ Assign2($1, $3) } 

matrix_rev_list:
  matrix_list 	{ List.rev $1 }

matrix_list:
	LBRACE actuals_opt RBRACE 	{ [$2] }
|	matrix_list COMMA LBRACE actuals_opt RBRACE { $4::$1 }

actuals_opt:
  /*nothing*/	{ [] }
| actuals_list	{ List.rev $1 }

actuals_list:
  expr						{ [$1] }
| actuals_list COMMA expr	{$3 :: $1 }
