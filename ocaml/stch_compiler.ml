(* (* Stitch Compiler *)

let in_channel = open_in Sys.argv.(1) in

let lexbuf = Lexing.from_channel in_channel in

let program = Stch_parser.program Stch_scanner.token lexbuf in
let finalcast =  Stch_semantic.check_prog program in
let outprog = C_generator.string_of_c_program finalcast in
print_string outprog *)