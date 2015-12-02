open Stch_ast
open Stch_cast
exception Error of string

(* symbol table -> string *)
let string_of_symTable (syms: symTable) = let str = "SymTable: \n" ^ String.concat "\n" (List.map (fun (typ, name, _) -> "[" ^ Stch_ast.dataType typ ^ " " ^ name ^ "]") syms.vars) ^ "\n"
	in print_endline str

(* find a variable (and associated type) in the symbol table *)
let rec find_variable (scope: symTable) name = 
	try
		List.find (fun (_, s, _) -> s = name ) scope.vars
	with Not_found -> match scope.parent with
	Some(parent) -> find_variable parent name
	| _ -> raise (Error("Bad ID " ^ name)) (* in general, any type mismatch raises an error *)

(* type check binary operations *)
(* for now, Stitch does not support type coercion, so biops must be int/int or flt/flt  *)
let check_binop (lhs: dataType) (rhs: dataType) (env: stch_env) =
	match (lhs, rhs) with
	  (Int, Int) 		-> Int
	| (Float, Float) 	-> Float
	| (_, _) -> raise (Error("Incompatable data types for binop"))

(* type check an expression and put into c_ast *)
let rec check_expr (e: expr) (env: stch_env) : (Stch_cast.c_expr * Stch_ast.dataType) = 
	match e with
	(* primitives get a free pass *)
	  Int(l) 	-> C_Int(l), Int
	| Float(l) 	-> C_Float(l), Float
	| Char(l) 	-> C_Char(l), Char
	| Id(l) -> C_Id(l), Id
	| String(l) -> C_String(l), String
	(* other exprs need to call their respective check functions *)
	| Binop(lhs, o, rhs) -> binop_ret lhs o rhs env
	| Negate(l) -> C_Negate(l)
	| Call(f, b) -> check_call f b env
	| Assign2(lhs, rhs) -> check_assign2 lhs rhs env
	| Noexpr -> C_Noexpr, Void

	(* check the binop return type*)
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

	(* check assign2 (i.e. expr assign) *)
	and check_assign2 (lhs: string) (rhs: expr) (env: stch_env) : (Stch_cast.c_expr * Stch_ast.dataType) = 
		let (t1, _, _) = find_variable env.scope lhs
		and (rhs, t2) = check_expr rhs env in
		if t1 = t2 then
			C_Assign2(lhs, rhs), t2
		else
			raise (Error("Type mismatch on variable assignment"))

	(* check function call *)
	and check_call (f: string) (el: expr list) (env: stch_env) =
		let l_expr_typ = List.map (fun e -> check_expr e env) el in
		let func_rets = find_func env.funcs f in
		match func_rets with
		[] -> raise (Error("Invalid function: " ^ f))
		| _ -> let (l_expr, fdecl) = find_func_sig f l_expr_typ func_rets in
			C_Call(f, l_expr), fdecl.fdecl_type

	(* function signature verify *)
	and find_func_sig (f: string) (opts: (c_expr * dataType) list) (func_rets: c_fdecl list) = 
		try
			match func_rets with
				[] -> raise (Error("Signature mismatch on function call " ^ f))
				| hd::tl -> let formals = hd.fdecl_formals in
								let cexpr = List.map2 (fun (opt: c_expr * dataType) (formal: c_vdecl) ->
								let opt_typ = snd opt in
								let formal_type = c_vdecl.vdecl_type in
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

(* typecheck a statement *)
let rec check_stmt (s: Stch_ast.stmt) (env: stch_env) = match s with
	Block(ss) -> 
		let scope' = { parent = Some(env.scope); vars = []; } in
			let env' = { env with scope = scope' } in
			let ss = List.map (fun s -> check_stmt s env') (List.rev ss) in
			scope'.vars <- List.rev scope'.vars;
			C_Block(scope', ss)
	| Expr(e) -> let (e,t) = check_expr e env in C_Expr(e,t)
	| Return(e) -> check_return e env
	| If(e, s1, s2) -> check_if e s1 s2 env
	(* need to add check_for *)
	(* | For(e1, e2, s) -> check_for e1 e2 s env *)
	| While(e, s) -> check_while e s env
	| Stitch(e1, e2, e3, e4, s) -> C_Stitch(e1, e2, e3, e4, s)
	(* stmt assign needs to be fixed *)
	(* | Assign(v, e) -> check_var_decl v e env *)
	| Break -> C_Break

	(* typecheck return (not return type, but keywork 'return') *)
	and check_return (e: expr) (env: stch_env) =
		if env.in_func then
		let (e,t) = check_expr e env in
			if t = env.retType then
				C_Return(e, t)
			else
				raise (Error("Incompatable return type. Expected type " ^ string_of_dataType env.retType ^ ", found type " ^ string_of_dataType t))
		else
			raise (Error("Invalid 'return' call"))

	(* typecheck the while loop *)
	and check_while (e: expr) (s: stmt) (env: stch_env) =
		let (e,t) = check_expr e env in
		if t = Bool then
			let s' = check_stmt s env in C_While(e,s')
		else
			raise (Error("Invalid 'while' expression"))

(* typecheck a function decleration *)
let check_fdecl (func: Stch_ast.fdecl) (env: stch_env) : c_fdecl =
	if env.in_func then
		raise (Error ("Cannot declare a funtion within another function"))
	else
		let env' = { env with scope = {parent = Some(env.scope); vars = [];}
		ret_type = func.fdecl_type; in_func = true} in
		let formals = (List.rev (List.map (fun x -> check_formals x env') func.formals)) in
		let f = { Stch_cast.fdecl_name = func.fdecl_name; Stch_cast.fdecl_type = func.fdecl_type; Stch_cast.fdecl_formals = func.fdecl_formals; Stch_cast.body = (List.map (fun x -> check_stmt x env') func.body );} in
		env.funcs <- f::env.funcs; f 

(* typecheck the ast env *)
let init_env : (stch_env) = 
	let init_funcs = [] in (* Need to add builtin functions here *)
	let init_scope = { parent = None; vars = []; } in
	{ funcs = init_funcs; scope = init_scope; retType = Void; in_func = false; }

(* check the programc *)
let check_prog (prog: Stch_ast.program) : (Stch_cast.c_prgram) = 
	let env = init_env in 
	{ Stch_cast.stms = (List.map (fun x -> check_stmt x env)); Stch_cast.funcs = (List.map (fun x -> check_fdecl x env) prog.funcs); Stch_cast.syms = env.scope}
