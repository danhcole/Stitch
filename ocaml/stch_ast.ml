type op = Add | Subtract | Times | Divide | Mod | Equal | Ne | Lt | Le | Gt | Ge 
          | Or | And

type vdecl = {
  vdecl_type     : string;
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
  | Noexpr

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
  | Assign2 of string * expr
  | Break

type fdecl = {
    ftype : string;
    fname : string;
    formals : vdecl list;
    body : stmt list;
  }

type program = vdecl list * fdecl list


let rec string_of_expr = function
    Int(l) -> string_of_int l
  | Float(l) -> string_of_float l
  | Char(l) -> String.make 1 l
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
  | Call(f, el) -> (match f with "print" -> "stch_print" | _ -> f) ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""

let string_of_vdecl vdecl = vdecl.vdecl_type ^ " " ^ vdecl.vdecl_name ^ (* " " ^ vdecl.array_size ^ *) ";\n"

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Vdecl(v) -> v.vdecl_type ^ " " ^ v.vdecl_name ^ ";\n";
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
        string_of_expr e3 ^ " by " ^ string_of_expr e4 ^ " " ^ string_of_stmt s
  | Assign(v, e) -> string_of_vdecl v ^ " = " ^ string_of_expr e ^ ";\n"
  | Assign2(i, e) -> i ^ " = " ^ string_of_expr e ^ ";\n"
  | Break -> "break;"

let string_of_fdecl fdecl =
  fdecl.ftype ^ " " ^ fdecl.fname ^ "(" ^ String.concat ", " (List.map string_of_vdecl fdecl.formals) ^ ")\n{\n" ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)

