open Stch_ast
open Stch_cast
exception Error of string

type stch_name_gen = { mutable name : int }
let sn = {name = 0;}

(* symbol table -> string *)
let string_of_symTable (syms: symTable) = let str = "SymTable: \n" ^ 
				String.concat "\n" (List.map (fun (typ, name, _) -> "[" ^ 
				Stch_ast.string_of_dataType typ ^ " " ^ name ^ "]") syms.vars) ^ "\n"
	in print_endline str

(* find a variable (and associated type) in the symbol table *)
let rec find_variable (scope: symTable) name = 
	try
		List.find (fun (_, s, _) -> s = name ) scope.vars
	with Not_found -> match scope.parent with
	Some(parent) -> find_variable parent name
	| _ -> raise (Error("Bad ID " ^ name)) (* in general, any type mismatch raises an error *)

(* check to see if a function has been defined *)
let rec find_func (funcs: c_fdecl list) fname =
	try
		List.find ( fun fn -> fn.fdecl_name = fname ) funcs
	with Not_found -> raise (Error ("Function call not recognized: " ^ fname))

(* type check binary operations *)
(* for now, Stitch does not support type coercion, so binops must be int/int or flt/flt  *)
let check_binop (lhs: dataType) (rhs: dataType) (env: stch_env) : (Stch_ast.dataType) =
	match (lhs, rhs) with
	  (Tint, Tint) 		-> Tint
	| (Tfloat, Tfloat) 	-> Tfloat
	| (_, _) -> raise (Error("Incompatable data types for binop"))

(* check variable decleration, returns a C_Vdecl *)
let check_vdecl (decl: vdecl) (env: stch_env) = 
	let invalid = List.exists (fun (_, s, _) -> s = decl.vdecl_name) env.scope.vars in 
		if invalid then
			raise (Error("Variable already declared"))
		else
			env.scope.vars <- (decl.vdecl_type, decl.vdecl_name, C_Noexpr)::env.scope.vars;
			let v = { Stch_cast.vdecl_type = decl.vdecl_type; 
						Stch_cast.vdecl_name = decl.vdecl_name } in 
				C_Vdecl(v)

(* same as check_vdecl, except that it returns a triple of vdecl, datatype, name *)
let check_vdecl_t (decl: vdecl) (env: stch_env) = 
	let invalid = List.exists (fun (_, s, _) -> s = decl.vdecl_name) env.scope.vars in 
		if invalid then
			raise (Error("Variable already declared"))
		else
			env.scope.vars <- (decl.vdecl_type, decl.vdecl_name, C_Noexpr)::env.scope.vars;
			let v = { Stch_cast.vdecl_type = decl.vdecl_type; 
						Stch_cast.vdecl_name = decl.vdecl_name } in 
				v, v.vdecl_type, v.vdecl_name

(* type check an expression and put into c_ast *)
let rec check_expr (e: expr) (env: stch_env) : (Stch_cast.c_expr * Stch_ast.dataType) = 
	match e with
	(* primitives get a free pass *)
	  Int(l) 	-> C_Int(l), Tint
	| Float(l) 	-> C_Float(l), Tfloat
	| Char(l) 	-> C_Char(l), Tchar
	| Escape(l) -> C_Escape(l), Tchar
	| String(l) -> C_String(l), Tstring
	(* For ID's, check to see if the variable has been declared, if it has, get the name and type *)
	| Id(l) -> 
		let var = try find_variable env.scope l
			with Not_found -> raise(Error("Undefined Identifier" ^ l))
				in
				let (typ, vname, _) = var in
				C_Id(vname, typ), typ
	(* other exprs need to call their respective check functions *)
	| Binop(lhs, o, rhs) -> binop_ret lhs o rhs env
	| Negate(l) -> check_negate l env
	| Call(f, b) -> check_call f b env
	| Assign2(lhs, rhs) -> check_assign2 lhs rhs env
	| Array_Index_Access(name, index) -> check_array_index name index env
	| Array_Item_Assign(name, index, ex) -> check_array_item_assign name index ex env
	| Matrix_Index_Access(name, row, col) -> check_matrix_index name row col env
	| Matrix_Item_Assign(name, row, col, ex) -> check_matrix_item_assign name row col ex env
	| Noexpr -> C_Noexpr, Tvoid
	| _ -> C_Noexpr, Tvoid  (* Can remove when everything else is added *)

	(* check negation.  As of now, only ints and floats can be negated *)
	and check_negate (e: expr) (env: stch_env) = 
		let exp = check_expr e env in
		match snd exp with
		  Tint -> C_Negate((fst exp)), Tint
		| Tfloat -> C_Negate((fst exp)), Tfloat
		| _ -> raise (Error("Cannot negate type " ^ string_of_dataType (snd exp)))

	(* check the binop return type*)
	and binop_ret (lhs: expr) (o: op) (rhs: expr) (env: stch_env) : (Stch_cast.c_expr * Stch_ast.dataType) =
		let (lhs, t1) = check_expr lhs env
		and (rhs, t2) = check_expr rhs env in

		match o with
		  Add -> C_Binop(lhs, o, rhs), check_binop t1 t2 env
		| Subtract -> C_Binop(lhs, o, rhs), check_binop t1 t2 env
		| Times -> C_Binop(lhs, o, rhs), check_binop t1 t2 env
		| Divide -> C_Binop(lhs, o, rhs), check_binop t1 t2 env
		| Mod -> C_Binop(lhs, o, rhs), check_binop t1 t2 env
		| Equal -> C_Binop(lhs, o, rhs), check_binop t1 t2 env
		| Ne -> C_Binop(lhs, o, rhs), check_binop t1 t2 env
		| Lt -> C_Binop(lhs, o, rhs), check_binop t1 t2 env
		| Le -> C_Binop(lhs, o, rhs), check_binop t1 t2 env
		| Gt -> C_Binop(lhs, o, rhs), check_binop t1 t2 env
		| Ge -> C_Binop(lhs, o, rhs), check_binop t1 t2 env
		| Or -> C_Binop(lhs, o, rhs), check_binop t1 t2 env
		| And -> C_Binop(lhs, o, rhs), check_binop t1 t2 env

	(* check assign2 (i.e. expr assign) *)
	and check_assign2 (lhs: string) (rhs: expr) (env: stch_env) : (Stch_cast.c_expr * Stch_ast.dataType) = 
		let (t1, _, _) = find_variable env.scope lhs
		and (rhs, t2) = check_expr rhs env in
		if t1 = t2 then
			C_Assign2(lhs, rhs), t2
		else if t1 = Tint && t2 = Tchar then
			C_Assign2(lhs, rhs), t1
		else
			raise (Error("Type mismatch on variable assignment " ^ lhs ^ 
				"\nExpected: " ^ string_of_dataType t1 ^ " Got: " ^ string_of_dataType t2))

	(* Checking array access by index. Index should be an int, we just need to make sure that the
		array exists. We could also conceviably rewrite this later to do bounds checking *)
	and check_array_index (n: string) (index: expr) (env: stch_env) =
		let var = find_variable env.scope n in
		let (typ, vname, _) = var in
		let (e, t) = check_expr index env in match t with
			Tint -> C_Array_Index(vname, e, typ), typ
			| _ -> raise(Error("Cannot index into an array with type " ^ string_of_dataType t))

	(*  Checking matrix access by indices. They should both be ints, row and col.
		Also we need to check that the variable exists first
		 *)
	and check_matrix_index (n: string) (row: expr) (col: expr) (env: stch_env) =
		let var = find_variable env.scope n in
		let (typ, vname, _) = var in
		let (erow, trow) = check_expr row env in
		let (ecol, tcol) = check_expr col env in match (trow, tcol) with
			(Tint, Tint) -> C_Matrix_Index(vname, erow, ecol, typ), typ
			| _ -> raise(Error("Cannot index into an array with types " ^ string_of_expr row ^ ", " ^ 
								string_of_expr col))
		

	(* Checking the array assignment to a specific index. Will validate the lhs as a valid access, and
		then will make sure the rhs has the proper type for assignment
	*)
	and check_array_item_assign (name: string) (index: expr) (rhs: expr) (env: stch_env) =
		let var = find_variable env.scope name in
		let (typ, vname, _) = var in
		let (e, t) = check_expr index env in
			if t <> Tint then
				raise(Error("Cannot index into an array with type " ^ string_of_dataType t))
			else
				let (erhs, trhs) = check_expr rhs env in
					if trhs <> typ then
						raise(Error("Type mismatch on array item assignment"))
					else
						C_Array_Item_Assign(vname, e, erhs), typ

	and check_matrix_item_assign (name: string) (row: expr) (col: expr) (rhs: expr) (env: stch_env) =
		let var = find_variable env.scope name in
		let (vtyp, vname, _) = var in
		let (erow, trow) = check_expr row env in
		let (ecol, tcol) = check_expr col env in
			if trow <> Tint || tcol <> Tint then
				raise(Error("Cannot index into a matrix with non-int values"))
			else
				let (erhs, trhs) = check_expr rhs env in
					if trhs <> vtyp then
						raise(Error("Type mismatch on matrix item assignment"))
					else
						C_Matrix_Item_Assign(vname, erow, ecol, erhs), vtyp

	(* check function call *)
	and check_call (f: string) (el: expr list) (env: stch_env) =
		let l_expr_typ = List.map (fun e -> check_expr e env) el in
		let func_ret = find_func env.funcs f in
		let args_l = find_func_sig f l_expr_typ func_ret in
			C_Call(func_ret.fdecl_name, args_l), func_ret.fdecl_type

	(* function signature verify *)
	and find_func_sig (f: string) (opts: (c_expr * dataType) list) (func_ret: c_fdecl) = match f with
		(* special handling for built-in functions
		   Not all built-ins need this (eg exit() ) *)
		 "print" -> (let arg = List.hd opts in
						match (snd arg) with
						 | Tint -> (fst arg)::[]
						 | Tfloat -> (fst arg)::[]
						 | Tchar -> (fst arg)::[]
						 | Tstring -> (fst arg)::[]
						 | _ -> raise (Error("Invalid print type: " ^ string_of_dataType (snd arg))))
		| "error" -> (let arg = List.hd opts in
						match (snd arg) with
						 | Tint -> (fst arg)::[]
						 | Tfloat -> (fst arg)::[]
						 | Tchar -> (fst arg)::[]
						 | Tstring -> (fst arg)::[]
						 | _ -> raise (Error("Invalid error type: " ^ string_of_dataType (snd arg))))
		(* All other functions *)
		| _ -> try
				let formals = func_ret.fdecl_formals in
					let cexpr = List.map2 (fun (opt: c_expr * dataType) (formal: c_vdecl) ->
						let opt_typ = snd opt in
						let formal_type = formal.vdecl_type in
							if opt_typ = formal_type then
								fst opt
							else
								C_Noexpr) opts formals in
						let matched = List.exists (fun e -> e = C_Noexpr) cexpr in
						if matched then
							find_func_sig f opts func_ret
						else
							cexpr
			with Invalid_argument(x) ->
				raise (Error("Wrong number of args in function call " ^ f))

(* Helper function for array initialization. This function will recursively traverse a list of
	expressions and try to type match them with the type of the array they're being added into.
	This function is called from check_array_init further down in the code
*)
let rec check_init_vals (name: arraydecl) (el: expr list) (t: dataType) (env: stch_env) = 
	match el with
		| [] -> name
		| head::tail -> let (ex, typ) = check_expr head env in
			if typ = t then
				check_init_vals name tail typ env
			else
				raise(Error("Types of array initialization do not match"))

let rec check_matrix_rows (name: matrixdecl) (el: expr list) (t: dataType) (env: stch_env) =
	match el with
		| [] -> name
		| head::tail -> let (exp, typ) = check_expr head env in
			if typ = t then begin
				(* print_string "CHECKING MATRIX TYPES\n"; *)
				check_matrix_rows name tail typ env
			end
			else 
				raise(Error("Types of matrix init do not match"))

let rec check_matrix_vals (name: matrixdecl) (el: expr list list) (ncols: int) (t: dataType) (env: stch_env) =
	match el with
		| [] -> name
		| head::tail ->
			if ncols <> List.length head then begin
				(* print_string "COLS LENGTH NOT ACCURATE\n"; *)
				raise(Error("Rows are not matching length in matrix decl"))
			end
			else
				let m = check_matrix_rows name head t env in
				check_matrix_vals m tail ncols t env

(* Generate the names for the struct and the anonymous pthread functions *)
let gen_name (sn : stch_name_gen) = 
	let i = sn.name in 
		sn.name <- i+1; "_" ^ string_of_int i

let get_id_from_expr (ex: expr) = match ex with
	Id(l) -> l
	|_ -> "_null"

(* typecheck a statement *)
let rec check_stmt (s: Stch_ast.stmt) (env: stch_env) = match s with
	Block(ss) -> 
		let scope' = { parent = Some(env.scope); vars = []; } in
			let env' = { env with scope = scope' } in
			let ss = List.map (fun s -> check_stmt s env') ss in
			scope'.vars <- List.rev scope'.vars;
			C_Block(scope', ss)
	| Vdecl(v) -> check_vdecl v env
	| Expr(e) -> let (e,t) = check_expr e env in C_Expr(t, e)
	| ArrayDecl(a) -> check_array_decl a env
	| ArrayInit(a, el) -> check_array_init a el env
	| MatrixDecl(m) -> check_matrix_decl m env
	| MatrixInit(mdecl, el) -> check_matrix_init mdecl el env
	| Return(e) -> check_return e env
	| If(e, s1, s2) -> check_if e s1 s2 env
	| For(e1, e2, e3, s) -> check_for e1 e2 e3 s env
	| While(e, s) -> check_while e s env
	| Stitch(e1, e2, e3, e4, s) -> check_stitch e1 e2 e3 e4 s env
	(* stmt assign needs to be fixed *)
	| Assign(v, e) -> check_assign v e env
	| Break -> C_Break

	(* check assign (i.e. stmt assign) *)
	and check_assign (lhs: vdecl) (rhs: expr) (env: stch_env) = 
		let (v, t1, _) = check_vdecl_t lhs env
			and (rhs, t2) = check_expr rhs env in
		if t1 = t2 then
			C_Assign(v, rhs)
	else
		raise (Error("Type mismatch on variable assignment " ^ string_of_vdecl lhs))

	(* typecheck return (not return type, but keyword 'return') *)
	and check_return (e: expr) (env: stch_env) =
		if env.in_func then
		let (e,t) = check_expr e env in
			if t = env.retType then
				C_Return(t, e)
			else
				raise (Error("Incompatable return type. Expected type " ^ 
					string_of_dataType env.retType ^ 
					", found type " ^ 
					string_of_dataType t))
		else
			raise (Error("Invalid 'return' call"))

	and check_array_decl (a: arraydecl) (env : stch_env) =

		(* create a variable declaration out of the array declaration so we can check for it *)
		let ve = { Stch_ast.vdecl_type = a.arraydecl_type; 
					Stch_ast.vdecl_name = a.arraydecl_name} in

		(* check to see if the variable is not already declared *)
		let invalid = List.exists (fun (_, s, _) -> s = ve.vdecl_name) env.scope.vars in 
		if invalid then
			raise (Error("Variable " ^ ve.vdecl_name ^ " already declared"))
		else
		(* if it isn't, put it in the scope, and make a new c_arraydecl 
			after you typematch the size expression *)

		(* If we have an arraydecl, we want the C_EXPR in the symtable to be an index operation, so we can
			get the size information when we are passing the symtable to the code generator 
			This is a bit hacky, but it should work for what we need it to
		*)
			let (ex, ty) = check_expr a.arraydecl_size env in 
			env.scope.vars <- (ve.vdecl_type, ve.vdecl_name, C_Array_Index(ve.vdecl_name, ex, ve.vdecl_type))::env.scope.vars;
			let (ex, typ) = check_expr a.arraydecl_size env in
			match typ with 
				Tfloat -> raise (Error("Invalid array size type, expects int"))
				| Tchar -> raise (Error("Invalid array size type, expects int"))
				| Tstring -> raise (Error("Invalid array size type, expects int"))
				| Tvoid -> raise (Error("Invalid array size type, expects int"))
			(* else it's a void or an int, and it's allowed *)
				| _ -> let v = { Stch_cast.arraydecl_type = ve.vdecl_type; 
									Stch_cast.arraydecl_name = ve.vdecl_name; 
									Stch_cast.arraydecl_size = a.arraydecl_size} in C_ArrayDecl(v)

	(* checking the array initialization. This will be done in 3 steps
		1. Check to see if the array can be declared as a new variable
		2. Make sure that all the args in the list are the same type
		3. Make sure that the type in the list matches the type
		4. Make sure that the size of the list matches the size of the decl (low priority for now) 
	*)
	and check_array_init (a: arraydecl) (el: expr list) (env: stch_env) =
		(* first step: check that we have a valid array decl *)
		let invalid = List.exists (fun (_,s,_) -> s = a.arraydecl_name) env.scope.vars in
			if invalid then
				raise (Error("Variable " ^ a.arraydecl_name ^ " already declared"))
			else begin
				let (ex, ty) = check_expr a.arraydecl_size env in 
				env.scope.vars <- (a.arraydecl_type, a.arraydecl_name,
					 C_Array_Index(a.arraydecl_name, ex, a.arraydecl_type))::env.scope.vars;

				(* now that we know it's valid, check the types of the list *)
				let s = a.arraydecl_size in 
				let i = string_of_expr s in
				let typ = a.arraydecl_type in
				(* try to match the init size with the list size. 
				   Init size must be an int constant, by C rules *)
				try 
				if int_of_string i = List.length el then
				let ret = check_init_vals a el typ env in 
					if ret = a then
						C_ArrayInit({Stch_cast.arraydecl_name = a.arraydecl_name;
									 Stch_cast.arraydecl_type = a.arraydecl_type;
									 Stch_cast.arraydecl_size = a.arraydecl_size;}, el)
					else
						raise(Error("Error parsing the list of array init args"))
				else
					raise(Error("Size mismatch in array initialization"))
				with
				| _ -> raise(Error("Cannot initialize array with a variable")) 
		end
	and check_matrix_init (m: matrixdecl) (el: expr list list) (env: stch_env) =
		(* First, we need to check that we have a valid declaration by checking for vdecl_t *)
		(* Check the size of the cols and rows, make sure they match the list counts
			rows = total # of sublists
			cols = length of the sublists (must be all the same length) 
		*)

		let invalid = List.exists (fun (_,s,_) -> s = m.matrixdecl_name) env.scope.vars in
			if invalid then
				raise (Error("Variable " ^ m.matrixdecl_name ^ " already declared"))
			else begin
				let (exr, ty) = check_expr m.matrixdecl_rows env in
				let (exc, ty2) = check_expr m.matrixdecl_cols env in
				env.scope.vars <- (m.matrixdecl_type, m.matrixdecl_name,
					 C_Matrix_Index(m.matrixdecl_name, exr, exc, m.matrixdecl_type))::env.scope.vars;
				let typ = m.matrixdecl_type in
				let errorstring = "Error with " in
				let rows = string_of_expr m.matrixdecl_rows in
				let cols = string_of_expr m.matrixdecl_cols in
				try
				if int_of_string rows = List.length el && int_of_string cols > -1 then
					(* Inside here need to call my functions from above for matrix stuff *)
					let ret = check_matrix_vals m el (int_of_string cols) typ env in
						if ret = m then
							C_MatrixInit( {Stch_cast.matrixdecl_name = m.matrixdecl_name;
								Stch_cast.matrixdecl_type = m.matrixdecl_type;
								Stch_cast.matrixdecl_rows = m.matrixdecl_rows;
								Stch_cast.matrixdecl_cols = m.matrixdecl_cols}, el)
						else begin
							(* print_string "HELLO"; *)
							raise(Error(errorstring ^ "checking return value of list iter"))
						end
				else begin
					(* print_string "HELLO2"; *)
					raise(Error(errorstring ^ "Int of string statement failure"))
				end
				with
				| _ -> begin
					(* print_string "HELLO3"; *)
					raise(Error(errorstring ^ "try/with failure"))
					end
			end

	and check_matrix_decl (m: matrixdecl) (env: stch_env) =

		(* create a variable declaration out of the array declaration so we can check for it *)
		let mat = { Stch_ast.vdecl_type = m.matrixdecl_type; 
					Stch_ast.vdecl_name = m.matrixdecl_name} in


		(* check to see if the variable is not already declared *)
		let invalid = List.exists (fun (_, s, _) -> s = mat.vdecl_name) env.scope.vars in 
		if invalid then
			raise (Error("Variable " ^ mat.vdecl_name ^ " already declared"))
		else
		(* if it isn't, put it in the scope, and make a new c_arraydecl 
			after you typematch the size expression *)
			let (exr, ty) = check_expr m.matrixdecl_rows env in 
			let (exc, ty) = check_expr m.matrixdecl_cols env in
			env.scope.vars <- (mat.vdecl_type, mat.vdecl_name,
				C_Matrix_Index(mat.vdecl_name, exr, exc ,mat.vdecl_type))::env.scope.vars;
			let (row, typerow) = check_expr m.matrixdecl_rows env in
			let (col, typecol) = check_expr m.matrixdecl_cols env in
			match (typerow, typecol) with 
				(Tfloat, _) -> raise (Error("Invalid matrix row type, expects int"))
				| (Tchar, _) -> raise (Error("Invalid matrix row type, expects int"))
				| (Tstring, _) -> raise (Error("Invalid matrix row type, expects int"))
				| (_, Tstring) -> raise (Error("Invalid matrix col type, expects int"))
				| (_, Tfloat) -> raise (Error("Invalid matrix col type, expects int"))
				| (_, Tchar) -> raise (Error("Invalid matrix col type, expects int"))
				| (Tint, Tvoid) -> raise (Error("Invalid matrix row type, expects int"))
				| (Tvoid, Tint) -> raise (Error("Invalid matrix row type, expects int"))
				| (Tvoid, Tvoid) -> raise (Error("Invalid matrix decl. Must be 2 ints"))
			(* else it's a void or an int, and it's allowed *)
				| _ -> let v = { Stch_cast.matrixdecl_type = mat.vdecl_type; 
								Stch_cast.matrixdecl_name = mat.vdecl_name;
								Stch_cast.matrixdecl_rows = m.matrixdecl_rows; 
								Stch_cast.matrixdecl_cols = m.matrixdecl_cols} in 
									C_MatrixDecl(v)


	(* Typechecking the expression of an "if" statement *)
	and check_if (ex: expr) (th: stmt) (el: stmt) (en : stch_env) =
		let (e, t) = check_expr ex en in
			if t = Tint || t = Tfloat || t = Tchar then
				let s1 = check_stmt th en in
				let s2 = check_stmt el en in
				C_If(e, s1, s2)
			else
				raise (Error("If clause has expression of type " ^ string_of_dataType t))


	(* typecheck the for loop *)
	and check_for (e1: expr) (e2: expr) (e3: expr) (st: stmt) (env: stch_env) =
		let (ex1, t1) = check_expr e1 env in
		let (ex2, t2) = check_expr e2 env in
		let (ex3, t3) = check_expr e3 env in
		if t1 <> Tint && t1 <> Tvoid then 
			raise (Error("For Loop: First expression not of type int."))
		else begin
			if t2 <> Tint && t2 <> Tvoid then 
				raise (Error("For Loop: Second expression not of type int."))
			else begin
				if t3 <> Tint && t3 <> Tvoid then 
					raise (Error("For Loop: Third expression not of type int."))
				else begin
					let s = check_stmt st env in
					C_For(ex1,ex2,ex3,s)
				end
			end
		end



(* Go through the body of a stitch loop and create an environment of all the variables used, so we know
	what needs to be passed in
	NOTE: VDECLS and ARRAYDECLS/MATRIXDECLS should NOT be added here, because those are local in the stitch
	loop and should not be copied *)

	and check_stitch_body (el: c_stmt list) (table: symTable) (env: stch_env) = match el with
	[] -> table
	| head::tail -> (* string_of_symTable table; *)
	(match head with
		(* The symtable of block here consists of all the variables that I do not want to put in the struct,
			so we just pass the list through *)
		| C_Block(t, b) -> check_stitch_body b table env
		| C_Vdecl(a) -> let n = a.vdecl_name in
			let table' = {Stch_cast.parent = table.parent; Stch_cast.vars = 
			List.filter ( fun (typ,nm,ex) -> nm <> n ) env.scope.vars } in 
			check_stitch_body tail table' env
		| C_ArrayDecl(a) -> let n = a.arraydecl_name in
			let table' = {Stch_cast.parent = table.parent; Stch_cast.vars = 
			List.filter ( fun (typ,nm,ex) -> nm <> n ) env.scope.vars } in 
			check_stitch_body tail table' env
		| C_MatrixDecl(m) -> let n = m.matrixdecl_name in
			let table' = {Stch_cast.parent = table.parent; Stch_cast.vars = 
			List.filter ( fun (typ,nm,ex) -> nm <> n ) env.scope.vars } in 
			check_stitch_body tail table' env
		| C_Assign(v, r) -> let n = v.vdecl_name in
			let table' = {Stch_cast.parent = table.parent; Stch_cast.vars = 
			List.filter ( fun (typ,nm,ex) -> nm <> n ) env.scope.vars } in 
			check_stitch_body tail table' env
		(* Need to add ARRAYINIT, MATRIXINIT, and others *)

		(* else I want to keep them in the symtable, continue down the list *)
		| _ -> check_stitch_body tail table env 
	)

(* 
	and iterate_vars (data: (dataType * string * c_expr) list ) (table: symTable) = match data with
		| [] -> table
		| head::tail ->  let b = (table.vars <- head::table.vars) in iterate_vars tail table

   	and check_all_envs (el: c_stmt list) (table: symTable) (env: stch_env) = match table.parent with
   		| None -> iterate_vars table.vars table
   		| Some(parent) ->  iterate_vars table.vars table; check_all_envs *)
 		


	(* Typechecking the expressions of a Stitch Loop *)
	and check_stitch (var : expr) (start : expr) (s_end : expr) (stride : expr) (body : stmt) (env : stch_env)  =
		let (var', t1) = check_expr var env in
		let name = get_id_from_expr var in
		let (start', t2) = check_expr start env in
		let (s_end', t3) = check_expr s_end env in
		let (stride', t4) = check_expr stride env in
		if t1 <> Tint then raise (Error("Stitch: First expression not of type int."))
		else begin
			if t2 <> Tint then raise (Error("Stitch: Second expression not of type int."))
			else begin
				if t3 <> Tint then raise (Error("Stitch: Third expression not of type int."))
				else begin
					if t4 <> Tint then raise (Error("Stitch: Fourth expression not of type int"))
					else begin 
						let body' = [(check_stmt body env)] in
						let t' = check_stitch_body body' env.scope env in
						let scope' = {Stch_cast.parent = env.scope.parent;
							Stch_cast.vars = List.filter (fun (t, n, e) -> n <> name) t'.vars } in
							C_Stitch(var', start', s_end', stride', gen_name sn, body', scope') 
					end
				end
			end
		end

	(* typecheck the while loop *)
	and check_while (e: expr) (s: stmt) (env: stch_env) =
		let (e,t) = check_expr e env in
		if t = Tint then
			let s' = check_stmt s env in C_While(e,s')
		else
			raise (Error("Invalid 'while' expression"))

let check_formals (decl: vdecl) (env: stch_env) = 
	match decl.vdecl_type with
		dataType -> env.scope.vars <- (decl.vdecl_type, decl.vdecl_name, C_Noexpr)::env.scope.vars;
			let v = { Stch_cast.vdecl_type = decl.vdecl_type; 
						Stch_cast.vdecl_name = decl.vdecl_name } in v

let check_for_ret (body: stmt list) = 
	if (List.exists ( fun ( s ) -> match s with
		 									Return(a) -> true
		 									| _ -> false ) body) then ""
	else
		raise (Error("Control reaches the end of nonvoid function."))

(* typecheck a function declaration *)
let check_fdecl (func: Stch_ast.fdecl) (env: stch_env) : c_fdecl =
	if env.in_func then
		raise (Error ("Cannot declare a function within another function"))
	else
		let env' = { env with scope = {parent = Some(env.scope); vars = [];};
		retType = func.fdecl_type; in_func = true} in
		let f_formals = (List.rev (List.map (fun x -> check_formals x env') func.fdecl_formals)) in
			let f = { Stch_cast.fdecl_name = func.fdecl_name; 
						Stch_cast.fdecl_type = func.fdecl_type; 
						Stch_cast.fdecl_formals = f_formals; 
						Stch_cast.body = ( List.map (fun x -> check_stmt x env') func.body );} in
							match func.fdecl_type with
								Tvoid -> env.funcs <- f::env.funcs; f 
								| _ -> ignore(check_for_ret func.body); env.funcs <- f::env.funcs; f 
							

(* typecheck the ast env *)
let init_env : (stch_env) = 
	let init_funcs = [{ fdecl_type = Tvoid;
						fdecl_name = "print";
						fdecl_formals = [ {vdecl_type = Tstring; vdecl_name = "c"}; ];
						body = [];
						};

						{fdecl_type = Tvoid;
						 fdecl_name = "error";
						 fdecl_formals = [ {vdecl_type = Tstring; vdecl_name = "c"}; ];
						 body = [];
						};

						{fdecl_type = Tvoid;
						 fdecl_name = "exit";
						 fdecl_formals = [ {vdecl_type = Tint; vdecl_name = "c"}; ];
						 body = [];
						};

						{fdecl_type = Tfile;
						 fdecl_name = "open_r";
						 fdecl_formals = [ {vdecl_type = Tstring; vdecl_name = "fn"}; ];
						 body = [];
						};

						{fdecl_type = Tfile;
						 fdecl_name = "open_w";
						 fdecl_formals = [ {vdecl_type = Tstring; vdecl_name = "fn"}; ];
						 body = [];
						};

						{fdecl_type = Tint;
						 fdecl_name = "read";
						 fdecl_formals = [ {vdecl_type = Tfile; vdecl_name = "f"}; {vdecl_type = Tchar; vdecl_name = "a"}; ];
						 body = [];
						};

						{fdecl_type = Tint;
						 fdecl_name = "write";
						 fdecl_formals = [ {vdecl_type = Tfile; vdecl_name = "f"}; {vdecl_type = Tchar; vdecl_name = "a"}; ];
						 body = [];
						};
						] in (* Need to add builtin functions here *)
	let init_scope = { parent = None; vars = []; } in
	{ funcs = init_funcs; 
		scope = init_scope; 
		retType = Tvoid; 
		in_func = false; 
	}


(* check the programc *)
let check_prog (prog: Stch_ast.program) : (Stch_cast.c_program) = 
	let env = init_env in
{ Stch_cast.stmts = (List.map (fun x -> check_stmt x env) (fst prog));
  Stch_cast.funcs = (List.map (fun x -> check_fdecl x env) (List.rev (snd prog)));
  Stch_cast.syms = env.scope;
}










