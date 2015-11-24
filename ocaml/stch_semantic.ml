open Stch_ast
open Stch_cast
exception error of string

let string_of_symTable (syms: symTable) = let str = "SymTable: \n" ^ String.concat "\n" (List.map (fun (typ, name, _) -> "[" ^ Stch_ast.dataType typ ^ " " ^ name ^ "]") syms.vars) ^ "\n"
	int print_endline str

let rec find_variable (scope: symTable) name = 
	try
		List.find (fun (_, s, _) -> s = name scope.vars
	with Not_found -> match scope.parent with
	Some(parent) -> find_variable parent name
	| _ -> raise (Error("Bad ID " ^ name))

let check_binop (lhs: dataType) (rhs: dataType) (env: env) =
	match (lhs, rhs) with
	  (Int, Int) 		-> Int
	| (Float, Float) 	-> Float
	| (_, _) -> raise (Error("Incompatable data types for binop"))

let rec check_expr (e: expr) (env: env) : (Stch_cast.c_expr * Stch_ast.dataType) = 
	match e with
	  Int(l) 	-> C_Int(l), Int
	| Float(l) 	-> C_Float(l), Float
	| Char(l) 	-> C_Char(l), Char
	| Id(l) -> C_Id(l), Id
	| String(l) -> C_String(l), String
	| Binop(lhs, o, rhs) -> check_binop lhs o rhs env
	| Negate(l) -> C_Negate(l)
	| Call(f, b) -> check_call f b env
	| Assign2(lhs, rhs) -> check_assign2 lhs rhs env
	| Noexpr -> C_Noexpr, Void

	and binop_ret (lhs: expr) (o: op) (rhs: expr) (env: env) : (Stch_cast.c_expr * Stch_ast.dataType) =
		let (lhs, t1) = check_expr lhs env
		and (rhs, t2) = check_expr rhs env in

		match o with
		