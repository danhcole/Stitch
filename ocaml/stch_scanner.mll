{ open Parser }

rule token = parse
	[' ' '\t' '\r' '\n'] { token lexbuf }
	| "//"						{ sline_comment lexbuf }
	| "/*"						{ block_comment lexbuf }
	| ';'						{ SEMI }
	| '''						{ SQUOTE }
	| '"'						{ DQUOTE }
	| '('						{ LPAREN }
	| ')'						{ RPAREN }
	| '['						{ LSQUARE }
	| ']'						{ RSQUARE }
	| '{'						{ LBRACE }
	| '}'						{ RBRACE }
	| ','						{ COMMA }
	| '*'						{ TIMES }
	| '/'						{ DIVIDE }
	| '+'						{ ADD }
	| "++"						{ INCREMENT }
	| '-'						{ SUBTRACT }
	| '%'						{ MOD }
	| '.'						{ ACCESS }
	| '='						{ ASSIGN }
	| "=="						{ EQUAL }
	| '!'						{ NEGATE }
	| "!="						{ NE }
	| "&&"						{ AND }
	| "||"						{ OR }
	| '&'						{ BAND }
	| '|'						{ BOR }
	| '~'						{ BNOT }
	| "<<"						{ LSHIFT }
	| ">>"						{ RSHIFT }
	| '>'						{ GT }
	| ">="						{ GE }
	| '<'						{ LT }
	| "<="						{ LE }
	| "if"						{ IF }x
	| "else"					{ ELSE }
	| "while"					{ WHILE }
	| "for"						{ FOR }
	| "break"					{ BREAK }
	| "return"					{ RETURN }
	| "const"					{ CONST }
	| "void"					{ VOID }
	| "int"						{ INT }
	| "float"					{ FLOAT }
	| "char"					{ CHAR }
	| "async"					{ ASYNC }
	| '-'?['0' - '9']+ as litr		{ INT(int_of_string intr) }
	| '-'?['0'-'9']'.'['0'-'9']* as litr { FLOAT(float_of_string intr) } (* fix - DHC*)
	| ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']* as litr { ID(litr) }
	| eof { EOF }
	| _ as char { raise (Failure("illegal character " ^ Char.escaped char))}

	and sline_comment = parse
		'\n'					{ token lexbuf }
		| _						{ sline_comment lexbuf }

	and block_comment = parse
		"*/"					{ token lexbuf}
		| _						{ block_comment lexbuf }