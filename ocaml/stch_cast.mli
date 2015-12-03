open Stch_ast

type c_expr =
	  C_Int of int
  | C_Float of float
  | C_Char of char
  | C_Id of string
  | C_String of string
  | C_Binop of c_expr * op * c_expr
  | C_Negate of c_expr
  | C_Call of string * c_expr list
  | C_Assign2 of string * c_expr
  | C_Noexpr

type symTable = {
	
	parent: symTable option;
	mutable vars: (dataType * string * c_expr) list;
}

type c_vdecl = {
	vdecl_type : dataType;
	vdecl_name : string;
}

type c_stmt =
    C_Block of symTable * c_stmt list
  | C_Vdecl of c_vdecl
  | C_Expr of dataType * c_expr
  | C_Return of dataType * c_expr
  | C_If of c_expr * Stch_ast.stmt * Stch_ast.stmt
  | C_For of c_expr * c_expr * c_expr * c_stmt
  | C_While of c_expr * c_stmt
  | C_Stitch of c_expr * c_expr * c_expr * c_expr * c_stmt
  | C_Assign of c_vdecl * c_expr
  | C_Break

type c_fdecl = {
    fdecl_type 		: dataType;
    fdecl_name 		: string;
    fdecl_formals 	: vdecl list;
    body 			: stmt list;
  }

type stch_env = {
	mutable funcs: c_fdecl list;
	scope: symTable;
	retType: dataType;
	in_func: bool;
}

type c_program = {
	stmts : c_stmt list;
	funcs : c_fdecl list;
	syms  : symTable;
}
