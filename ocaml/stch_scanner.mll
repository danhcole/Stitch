(* 
Stitch Scanner
December 2015
Authors: Dan Cole, Rashedul Haydar & Megan Skrypek

*)

{ open Stch_parser }

rule token = parse
	[' ' '\t' '\r' '\n'] { token lexbuf }
	| "//"						{ sline_comment lexbuf }
	| "/*"						{ block_comment lexbuf }
	| ';'						{ SEMI }
	| ':'						{ COLON }
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
	| '-'						{ SUBTRACT }
	| '%'						{ MOD }
	| '='						{ ASSIGN }
	| "=="						{ EQUAL }
	| '!'						{ NEGATE }
	| "!="						{ NE }
	| "&&"						{ AND }
	| "||"						{ OR }
	| '>'						{ GT }
	| ">="						{ GE }
	| '<'						{ LT }
	| "<="						{ LE }
	| "if"						{ IF }
	| "else"					{ ELSE }
	| "while"					{ WHILE }
	| "for"						{ FOR }
	| "stitch"					{ STITCH }
	| "from"					{ FROM }
	| "to"						{ TO }
	| "by"						{ BY }
	| "break"					{ BREAK }
	| "return"					{ RETURN }
	| "void"					{ TVOID }
	| "int"						{ TINT }
	| "float"					{ TFLOAT }
	| "char"					{ TCHAR }
	| "int_ap"					{ TINTAP }
	| "int_am"					{ TINTAM }
	| "float_ap"				{ TFLOATAP }
	| "float_am"				{ TFLOATAM }
	| "FILE"					{ TFILE }
	| ['-' '+']?['0' - '9']+ as i_litr					{ INT(int_of_string i_litr) }
	| ['-' '+']?['0'-'9']?'.'['0'-'9']* as f_litr 		{ FLOAT(float_of_string f_litr) }
	| '''([^''']  as ch_litr)'''						{ CHAR(ch_litr)}
	| '''(['\\']['\\' ''' '"' 't' 'n'] as esc_ch)'''		{ ESCAPE(esc_ch)}
	| '"'([^'"']* as st_litr)'"'						{ STRING(st_litr)}
	| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as litr { ID(litr) }
	| eof { EOF }
	| _ as char { raise (Failure("illegal character " ^ Char.escaped char))}

	and sline_comment = parse
		'\n'					{ token lexbuf }
		| _						{ sline_comment lexbuf }

	and block_comment = parse
		"*/"					{ token lexbuf }
		| _						{ block_comment lexbuf }
