let lexbuf = Lexing.from_channel stdin in
let program = Stch_parser.program Stch_scanner.token lexbuf in
let listing = Stch_ast.string_of_program program
		in print_string listing