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

let check_binop (lhs: dataType) (rhs: dataType) (env: stch_env) =
	match (lhs, rhs) with
	  (Int, Int) 		-> Int
	| (Float, Float) 	-> Float
	| (_, _) -> raise (Error("Incompatable data types for binop"))

let rec check_expr (e: expr) (env: stch_env) : (Stch_cast.c_expr * Stch_ast.dataType) = 
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

	and binop_ret (lhs: expr) (o: op) (rhs: expr) (env: stch_env) : (Stch_cast.c_expr * Stch_ast.dataType) =
		let (lhs, t1) = check_expr lhs env
		and (rhs, t2) = check_expr rhs env in

		match o with
		  Add -> C_Binop(lhs, o, rhs), check_binop lhs, rhs, o
		| Subtract -> C_Binop(lhs, o, rhs), check_binop lhs, rhs, o
		| Times -> C_Binop(lhs, o, rhs), check_binop lhs, rhs, o
		| Divide -> C_Binop(lhs, o, rhs), check_binop lhs, rhs, o
		| Mod -> C_Binop(lhs, o, rhs), check_binop lhs, rhs, o
		| Equal -> C_Binop(lhs, o, rhs), check_binop lhs, rhs, o
		| Ne -> C_Binop(lhs, o, rhs), check_binop lhs, rhs, o
		| Lt -> C_Binop(lhs, o, rhs), check_binop lhs, rhs, o
		| Le -> C_Binop(lhs, o, rhs), check_binop lhs, rhs, o
		| Gt -> C_Binop(lhs, o, rhs), check_binop lhs, rhs, o
		| Ge -> C_Binop(lhs, o, rhs), check_binop lhs, rhs, o
		| Or -> C_Binop(lhs, o, rhs), check_binop lhs, rhs, o
		| And -> C_Binop(lhs, o, rhs), check_binop lhs, rhs, o

	and check_assign2 (lhs: string) (rhs: expr) (env: stch_env) : (Stch_cast.c_expr * Stch_ast.dataType) = 
		let (t1, _, _) = find_variable env.scope lhs
		and (rhs, t2) = check_expr rhs env in
		if t1 = t2 then
			C_Assign2(lhs, rhs), t2
		else
			raise (Error("Type mismatch on variable assignment"))

	and check_call (f: string) (el: expr list) (env: stch_env) =
		let l_expr_typ = List.map (fun e -> check_expr e env) el in
		let func_rets = find_func env.funcs f in
		match func_rets with
		[] -> raise (Error("Invalid function: " ^ f))
		| _ -> let (l_expr, fdecl) = find_func_sig f l_expr_typ func_rets in
			C_Call(f, l_expr), fdecl.fdecl_type

	and find_func_sig (f: string) (opts: (c_expr * dataType) list) (func_rets: c_fdecl list) = 
		try
			match func_rets with
				[] -> raise (Error("Signature mismatch on function call " ^ f))
				| hd::tl -> let formals = hd.fdecl_formals in
								let cexpr = List.map2 (fun (opt: c_expr * dataType) (formal: c_vdecl)) ->
								let opt_typ = snd opt in
								let formal_type = (c_vdecl.vdecl_type in
									if opt_typ = formal_type then
										fst opt
									else
										C_Noexpr) opts formals in
								let matched = List.exists (fun e -> e = C_Noexpr) cexpr in
								if matched then
									find_func_sig f opts tl
								else
									cexpr, hd
		with Invalid_argument(x) ->
			raise (Error("Wrong number of args in function call " ^ f))

let rec check_stmt (s: Stch_ast.stmt) (env: stch_env) = match s with
	Block(ss) -> 
		let scope' = { parent = Some(env.scope); vars = []; } in
			let env' = { env with scope = scope' } in
			let ss = List.map (fun s -> check_stmt s env') (List.rev ss) in
			scope'.vars <- List.rev scope'.vars;
			C_Block(scope', ss)
	| Expr(e) -> let (e,t) = check_expr e env in C_Expr(e,t)
	| Return(e) -> check_return e env
	| If(p, s) -> 



