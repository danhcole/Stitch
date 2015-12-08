type op = Add | Subtract | Times | Divide | Mod | Equal | Ne | Lt | Le | Gt | Ge 
          | Or | And
type dataType = Tint | Tfloat | Tchar | Tvoid | Tstring

type vdecl = {
  vdecl_type     : dataType;
  vdecl_name     : string;
(*   array_size     : int; *)
}

type expr =
    Int of int
  | Float of float
  | Char of char
  | Id of string
  | String of string
  | Binop of expr * op * expr
  | Negate of expr
  | Call of string * expr list
  | Assign2 of string * expr
  | Array_Item_Assign of string * int * expr
  | Array_Index_Access of string * int
  | Access of string * string
  | Noexpr

type arraydecl = {
    arraydecl_type : dataType;
    arraydecl_name : string;
    arraydecl_size : expr;

  }

  type matrixdecl = {
    matrixdecl_type : dataType;
    matrixdecl_name : string;
    matrixdecl_rows : expr;
    matrixdecl_cols : expr;

  }

type stmt =
    Block of stmt list
  | Vdecl of vdecl
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt
  | Stitch of expr * expr * expr * expr * stmt
  | Assign of vdecl * expr
  | ArrayDecl of arraydecl
  | ArrayInit of arraydecl * expr list
  | MatrixDecl of matrixdecl
  | Break

type fdecl = {
    fdecl_type : dataType;
    fdecl_name : string;
    fdecl_formals : vdecl list;
    body : stmt list;
  }

type program = stmt list * fdecl list

let string_of_dataType = function
  Tint -> "int"
  | Tfloat -> "float"
  | Tchar -> "char"
  | Tvoid -> "void"
  | Tstring -> "char *"

let rec string_of_expr = function
    Int(l) -> string_of_int l
  | Float(l) -> string_of_float l
  | Char(l) ->  "\'" ^ String.make 1 l ^ "\'"
  | Id(s) -> s
  | String(s) -> "\"" ^ s ^ "\""
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^
      (match o with
	Add -> "+" | Subtract -> "-" | Times -> "*" | Divide -> "/"
      | Equal -> "==" | Ne -> "!="
      | Lt -> "<" | Le -> "<=" | Gt -> ">" | Ge -> ">="
      | Or -> "||" | And -> "&&" | Mod -> "%" ) ^ " " ^
      string_of_expr e2
  | Negate(e) -> "!" ^ string_of_expr e
  | Call(f, el) -> (match f with "print" -> "printf" | _ -> f) ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Assign2(i, e) -> i ^ " = " ^ string_of_expr e ^ ";\n"
  | Array_Item_Assign(id, ind, e) -> id ^ "[" ^ string_of_int ind ^"] = " ^ string_of_expr e
  | Array_Index_Access(id, index) -> id ^ "[" ^ string_of_int index ^ "]"
  | Access(f, s) -> f ^ "." ^ s 
  | Noexpr -> ""

let string_of_vdecl vdecl = string_of_dataType vdecl.vdecl_type ^ " " ^ vdecl.vdecl_name (* " " ^ vdecl.array_size ^ *)

let string_of_arraydecl arraydecl = string_of_dataType arraydecl.arraydecl_type ^ " " ^ arraydecl.arraydecl_name ^ "[" ^
    string_of_expr arraydecl.arraydecl_size ^ "]"

let string_of_matrixdecl m = string_of_dataType m.matrixdecl_type ^ " " ^ m.matrixdecl_name ^ "[" ^
    string_of_expr m.matrixdecl_rows ^ "][" ^ string_of_expr m.matrixdecl_cols ^ "]"

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Vdecl(v) -> string_of_dataType v.vdecl_type ^ " " ^ v.vdecl_name ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | Stitch(e1,e2,e3,e4,s) ->
      "stitch " ^ string_of_expr e1 ^ " from " ^ string_of_expr e2 ^ " to " ^
        string_of_expr e3 ^ " by " ^ string_of_expr e4 ^ " : " ^ string_of_stmt s
  | Assign(v, e) -> string_of_vdecl v ^ " = " ^ string_of_expr e ^ ";\n"
  | ArrayDecl(a) -> string_of_arraydecl a ^ ";\n"
  | ArrayInit(arraydecl, el) -> string_of_arraydecl arraydecl ^ " = {" ^ String.concat ", " (List.map string_of_expr el) ^ "};\n"
  | MatrixDecl(m) -> string_of_matrixdecl m ^ ";\n"
  | Break -> "break;"

let string_of_fdecl fdecl =
  string_of_dataType fdecl.fdecl_type ^ " " ^ fdecl.fdecl_name ^ "(" ^ String.concat ", " (List.map string_of_vdecl fdecl.fdecl_formals) ^ ")\n{\n" ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (stmts, funcs) =
  String.concat "" (List.map string_of_stmt stmts) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)

