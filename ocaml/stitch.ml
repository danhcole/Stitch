(*------------------------------------------------*)
(* Parse and print the program                    *)
(*------------------------------------------------*)

let filename = Sys.argv.(1) ^ ".c" in

let in_channel = open_in Sys.argv.(1) in

let lexbuf = Lexing.from_channel in_channel in

let program = Stch_parser.program Stch_scanner.token lexbuf in
let listing = Stch_ast.string_of_program program in
let headers = "#include \"stch_headers.h\"\n\n" ^ listing 
		in Printf.fprintf (open_out filename) "%s" headers