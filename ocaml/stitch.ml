(*------------------------------------------------*)
(* Parse and print the program                    *)
(*------------------------------------------------*)

let filename = Sys.argv.(1) ^ ".c" in

let in_channel = open_in Sys.argv.(1) in

let lexbuf = Lexing.from_channel in_channel in

let program = Stch_parser.program Stch_scanner.token lexbuf in
let finalcast =  Stch_semantic.check_prog program in
let outprog = C_generator.string_of_c_program finalcast in

let headers = "#include \"stch_headers.h\"\n\n" ^ outprog 
		in Printf.fprintf (open_out filename) "%s" headers