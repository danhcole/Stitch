%{ open stch_ast %}
%token SEMI SQUOTE DQUOTE LPAREN RPAREN LSQUARE RSQUARE LBRACE RBRACE
%token COMMA TIMES DIVIDE ADD INCREMENT SUBTRACT MOD
%token ACCESS ASSIGN EQUAL NEGATE NE
%token AND OR BAND BOR BNOT LSHIFT RSHIFT
%token GT LE LT LE
%token IF ELSE WHILE FOR BREAK RETURN
%token CONST VOID
%token <int>INT 
%token <float> FLOAT 
%token <string> ID
%token EOF

%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left BOR 
%left BAND
%left EQUAL NE
%left LT GT LE GE
%left LSHIFT RSHIFT
%left ADD SUBTRACT
%left TIMES DIVIDE MOD
%right BNOT NEGATE
%left ACCESS INCREMENT DECREMENT
%start program
%type <stch_ast.program> program

%%

program:
	{EOF}


