open Stch_ast
open Stch_cast

let string_of_c_dataType = function
  Tint -> "int"
  | Tfloat -> "float"
  | Tchar -> "char"
  | Tvoid -> "void"

let rec string_of_c_expr = function
    C_Int(l) -> string_of_int l
  | C_Float(l) -> string_of_float l
  | C_Char(l) ->  "\'" ^ String.make 1 l ^ "\'"
  | C_Id(s) -> s
  | C_String(s) -> "\"" ^ s ^ "\""
  | C_Binop(e1, o, e2) ->
      string_of_c_expr e1 ^ " " ^
      (match o with
	      Add -> "+" | Subtract -> "-" | Times -> "*" | Divide -> "/"
      | Equal -> "==" | Ne -> "!="
      | Lt -> "<" | Le -> "<=" | Gt -> ">" | Ge -> ">="
      | Or -> "||" | And -> "&&" | Mod -> "%" ) ^ " " ^
      string_of_c_expr e2
  | C_Negate(e) -> "!" ^ string_of_c_expr e
  | C_Call(f, el) -> (match f with "print" -> "printf" | _ -> f) ^ "(" ^ String.concat ", " (List.map string_of_c_expr el) ^ ")"
  | C_Assign2(i, e) -> i ^ " = " ^ string_of_c_expr e ^ ";\n"
  (*  Array_Item_Assign(id, ind, e) -> id ^ "[" ^ string_of_int ind ^"] = " ^ string_of_c_expr e ^ ";\n" *)
  (* | C_Access(f, s) -> f ^ "." ^ s  *)
  | C_Noexpr -> ""

let string_of_c_vdecl vdecl = string_of_c_dataType vdecl.vdecl_type ^ " " ^ vdecl.vdecl_name (* " " ^ vdecl.array_size ^ *)

(* let string_of_c_arraydecl arraydecl = string_of_c_dataType arraydecl.arraydecl_type ^ " " ^ arraydecl.arraydecl_name ^ "[" ^
    string_of_c_expr arraydecl.arraydecl_size ^ "]" *)

let rec string_of_c_stmt = function
    C_Block(_, stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_c_stmt stmts) ^ "}\n"
  | C_Expr(_, e) -> string_of_c_expr e ^ ";\n";
  | C_Vdecl(v) -> string_of_c_dataType v.vdecl_type ^ " " ^ v.vdecl_name ^ ";\n";
  | C_Return(_, c_expr) -> "return " ^ string_of_c_expr c_expr ^ ";\n";
  | C_If(e, s, C_Block(_, [])) -> "if (" ^ string_of_c_expr e ^ ")\n" ^ string_of_c_stmt s
  | C_If(e, s1, s2) ->  "if (" ^ string_of_c_expr e ^ ")\n" ^
      string_of_c_stmt s1 ^ "else\n" ^ string_of_c_stmt s2
  | C_For(e1, e2, e3, s) ->
      "for (" ^ string_of_c_expr e1  ^ " ; " ^ string_of_c_expr e2 ^ " ; " ^
      string_of_c_expr e3  ^ ") " ^ string_of_c_stmt s
  | C_While(e, s) -> "while (" ^ string_of_c_expr e ^ ") " ^ string_of_c_stmt s
  | C_Stitch(e1,e2,e3,e4,s) ->
      "stitch " ^ string_of_c_expr e1 ^ " from " ^ string_of_c_expr e2 ^ " to " ^
        string_of_c_expr e3 ^ " by " ^ string_of_c_expr e4 ^ " : " ^ string_of_c_stmt s
  | C_Assign(v, e) -> string_of_c_vdecl v ^ " = " ^ string_of_c_expr e ^ ";\n"
(*   | ArrayDecl(a) -> string_of_c_arraydecl a ^ ";\n" *)
  (*| ArrayAssign(arraydecl, el) -> "arraydecl;\n" *)
  | C_Break -> "break;"

let string_of_c_fdecl fdecl =
  string_of_c_dataType fdecl.fdecl_type ^ " " ^ fdecl.fdecl_name ^ "(" ^ String.concat ", " (List.map string_of_c_vdecl fdecl.fdecl_formals) ^ ")\n{\n" ^
  String.concat "" (List.map string_of_c_stmt fdecl.body) ^
  "}\n"

let string_of_c_program (stmts, funcs) =
  String.concat "" (List.map string_of_c_stmt stmts) ^ "\n" ^
  String.concat "\n" (List.map string_of_c_fdecl funcs)