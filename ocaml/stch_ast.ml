type op = Add | Subtract | Times | Divide | Mod | Equal | Ne | Lt | Le | Gt | Ge 
          | Or | And | Bor | Band | Lshift | Rshift

type vdecl = {
  vtype     :string;
  vname     :string;
}

type expr =
    Int of int
  | Float of float
  | Char of char
  | Id of string
  | Binop of expr * op * expr
  | Bnot of expr
  | Assign of vdecl * expr
  | Access of string * string
  | Increment of expr
  | Decrement of expr
  | Negate of expr
  | Call of string * expr list
  | Noexpr

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt
  | Stitch of expr * expr * expr * expr * stmt
  | Break

type func_decl = {
    fname : string;
    formals : string list;
    locals : vdecl list;
    body : stmt list;
  }

type program = string list * func_decl list

let string_of_vdecl vdecl = vdecl.vtype ^ " " ^ vdecl.vname 

let rec string_of_expr = function
    Int(l) -> string_of_int l
  | Float(l) -> string_of_float l
  | Char(l) -> String.make 1 l
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^
      (match o with
	Add -> "+" | Subtract -> "-" | Times -> "*" | Divide -> "/"
      | Equal -> "==" | Ne -> "!="
      | Lt -> "<" | Le -> "<=" | Gt -> ">" | Ge -> ">="
      | Or -> "||" | And -> "&&" | Bor -> "|" | Band -> "&" 
      | Lshift -> "<<" | Rshift -> ">>" | Mod -> "%" ) ^ " " ^
      string_of_expr e2
  | Bnot(e) -> "~" ^ string_of_expr e
  | Assign(v, e) -> string_of_vdecl v ^ " = " ^ string_of_expr e
  | Access(v1, v2) -> v1 ^ "." ^ v2
  | Increment(e) -> string_of_expr e ^ "++"
  | Decrement(e) -> string_of_expr e ^ "--"
  | Negate(e) -> "!" ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
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
  | Break -> ""

let string_of_fdecl fdecl =
  fdecl.fname ^ "(" ^ String.concat ", " fdecl.formals ^ ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^ ";\n" ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)
