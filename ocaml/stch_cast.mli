(* 
C AST
December 2015
Authors: Dan Cole & Tim Waterman

The C AST that will be generated from our semantic analysis
*)

open Stch_ast

(* Expressions *)
type c_expr =
	  C_Int of int
  | C_Float of float
  | C_Char of char
  | C_Escape of string
  | C_Id of string * dataType
  | C_String of string
  | C_Binop of c_expr * op * c_expr
  | C_Negate of c_expr
  | C_Call of string * c_expr list
  | C_Assign2 of string * c_expr
  | C_Array_Index of string * c_expr * dataType
  | C_Matrix_Index of string * c_expr * c_expr * dataType
  | C_Array_Item_Assign of string * c_expr * c_expr
  | C_Matrix_Item_Assign of string * c_expr * c_expr * c_expr
  | C_Noexpr

(* Symbol table to store variable and function names *)
type symTable = {	
	parent: symTable option;
	mutable vars: (dataType * string * c_expr) list;
}

type c_vdecl = {
	vdecl_type : dataType;
	vdecl_name : string;
}

(* Array and Matrix data types *)
type c_arraydecl = {
  arraydecl_type : dataType;
  arraydecl_name : string;
  arraydecl_size : expr;
}

type c_matrixdecl = {
  matrixdecl_type : dataType;
  matrixdecl_name : string;
  matrixdecl_rows : expr;
  matrixdecl_cols : expr;
  }

(* Statements *)
type c_stmt =
    C_Block of symTable * c_stmt list
  | C_Vdecl of c_vdecl
  | C_ArrayDecl of c_arraydecl
  | C_ArrayInit of c_arraydecl * expr list
  | C_MatrixInit of c_matrixdecl * expr list list
  | C_MatrixDecl of c_matrixdecl
  | C_Expr of dataType * c_expr
  | C_Return of dataType * c_expr
  | C_If of c_expr * c_stmt * c_stmt
  | C_For of c_expr * c_expr * c_expr * c_stmt
  | C_While of c_expr * c_stmt 
  | C_Stitch of c_expr * c_expr * c_expr * c_expr * string * c_stmt list * symTable
  | C_Assign of c_vdecl * c_expr
  | C_Break

type c_fdecl = {
    fdecl_type 		: dataType;
    fdecl_name 		: string;
    fdecl_formals 	: c_vdecl list;
    body 			: c_stmt list;
  }

(* Our environment *)
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


