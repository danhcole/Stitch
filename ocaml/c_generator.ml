open Stch_ast
open Stch_cast
exception Error of string

let string_of_c_dataType = function
  Tint -> "int"
  | Tfloat -> "float"
  | Tchar -> "char"
  | Tvoid -> "void"
  | Tstring -> "char *"

let rec string_of_c_expr = function
    C_Int(l) -> string_of_int l
  | C_Float(l) -> string_of_float l
  | C_Char(l) ->  "\'" ^ String.make 1 l ^ "\'"
  | C_Id(s, t) -> s
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
  | C_Call(f, el) -> (match f with "print" -> "printf" | "error" -> "fprintf" | _ -> f) ^ "(" ^ String.concat ", " (match f with "print" -> print_2_fprint (List.hd el) | "error" -> error_2_fprintf (List.hd el) | _ -> List.map string_of_c_expr el) ^ ")"
  | C_Assign2(i, e) -> i ^ " = " ^ string_of_c_expr e
  | C_Array_Item_Assign(id, ind, e) -> id ^ "[" ^ string_of_c_expr ind ^"] = " ^ string_of_c_expr e
  | C_Array_Index(a, i, t) -> a ^ "[" ^ string_of_c_expr i ^ "]"
  | C_Matrix_Index(m, r, c, t) -> m ^ "[" ^ string_of_c_expr r ^ "][" ^ string_of_c_expr c ^ "]"
  | C_Matrix_Item_Assign(m, r, c, e) -> m ^ "[" ^ string_of_c_expr r ^ "][" ^ string_of_c_expr c ^ "] = " ^ string_of_c_expr e
  (* | C_Access(f, s) -> f ^ "." ^ s  *)
  | C_Noexpr -> ""

      and print_2_fprint (e: c_expr) = match e with
        C_Int(l) -> ("\"%d\\n\", " ^ string_of_c_expr e)::[]
      | C_Float(l) -> ("\"%f\\n\", " ^ string_of_c_expr e)::[]
      | C_Char(l) -> ("\"%c\\n\", " ^ string_of_c_expr e)::[]
      | C_String(l) -> ("\"%s\\n\", " ^ string_of_c_expr e)::[]
      | C_Array_Index(a, i, t) -> (match t with
                                    Tint -> ("\"%d\\n\", " ^ a ^ "[" ^ string_of_c_expr i ^ "]")::[]
                                    | Tfloat -> ("\"%f\\n\", " ^ a ^ "[" ^ string_of_c_expr i ^ "]")::[]
                                    | Tchar -> ("\"%c\\n\", " ^ a ^ "[" ^ string_of_c_expr i ^ "]")::[]
                                    | Tstring -> ("\"%s\\n\", " ^ a ^ "[" ^ string_of_c_expr i ^ "]")::[]
                                    | Tvoid -> raise (Error("Invalid print type Void: " ^ a ^ "[" ^ string_of_c_expr i ^ "]")))
      | C_Matrix_Index(m, r, c, t) -> (match t with
                                     Tint -> ("\"%d\\n\", " ^ m ^ "[" ^ string_of_c_expr r ^ "][" ^ 
                                              string_of_c_expr c ^ "]")::[]
                                    | Tfloat -> ("\"%f\\n\", " ^ m ^ "[" ^ string_of_c_expr r ^ "][" ^
                                              string_of_c_expr c ^ "]")::[]
                                    | Tchar -> ("\"%c\\n\", " ^ m ^ "[" ^ string_of_c_expr r ^ "][" ^
                                              string_of_c_expr c ^ "]")::[]
                                    | Tstring -> ("\"%s\\n\", " ^ m ^ "[" ^ string_of_c_expr r ^ "][" ^
                                              string_of_c_expr c ^ "]")::[]
                                    | Tvoid -> raise(Error("Invalid print type void in matrix printing")))
      | C_Id(l, t) -> (match t with
                        Tint -> ("\"%d\\n\", " ^ string_of_c_expr e)::[]
                        | Tfloat -> ("\"%f\\n\", " ^ string_of_c_expr e)::[]
                        | Tchar -> ("\"%c\\n\", " ^ string_of_c_expr e)::[]
                        | Tstring -> ("\"%s\\n\", " ^ string_of_c_expr e)::[]
                        | Tvoid -> raise (Error("Invalid print type Void: " ^ string_of_c_expr e)))
      | C_Binop(lhs, o, rhs) -> (match o with
                                    Add -> (match lhs with
                                              C_Int(l) -> ("\"%d\\n\", " ^ string_of_c_expr lhs ^ "+" ^ string_of_c_expr rhs)::[]
                                            | C_Float(l) -> ("\"%f\\n\", " ^ string_of_c_expr lhs ^ "+" ^ string_of_c_expr rhs)::[]
                                            | C_Id(l, t) -> (match t with
                                                                  Tint -> ("\"%d\\n\", " ^ string_of_c_expr lhs ^ "+" ^ string_of_c_expr rhs)::[]
                                                                | Tfloat -> ("\"%f\\n\", " ^ string_of_c_expr lhs ^ "+" ^ string_of_c_expr rhs)::[]
                                                                | Tchar -> ("\"%c\\n\", " ^ string_of_c_expr lhs ^ "+" ^ string_of_c_expr rhs)::[]
                                                                | Tstring -> ("\"%s\\n\", " ^ string_of_c_expr lhs ^ "+" ^ string_of_c_expr rhs)::[]
                                                                | Tvoid -> raise (Error("Invalid print type Void: " ^ string_of_c_expr lhs ^ "+" ^ string_of_c_expr rhs)))
                                            | _ -> raise (Error("Invalid add in function call"))
                                          )
                                  | Subtract -> (match lhs with
                                              C_Int(l) -> ("\"%d\\n\", " ^ string_of_c_expr lhs ^ "-" ^ string_of_c_expr rhs)::[]
                                            | C_Float(l) -> ("\"%f\\n\", " ^ string_of_c_expr lhs ^ "-" ^ string_of_c_expr rhs)::[]
                                            | C_Id(l, t) -> (match t with
                                                                  Tint -> ("\"%d\\n\", " ^ string_of_c_expr lhs ^ "-" ^ string_of_c_expr rhs)::[]
                                                                | Tfloat -> ("\"%f\\n\", " ^ string_of_c_expr lhs ^ "-" ^ string_of_c_expr rhs)::[]
                                                                | Tchar -> ("\"%c\\n\", " ^ string_of_c_expr lhs ^ "-" ^ string_of_c_expr rhs)::[]
                                                                | Tstring -> ("\"%s\\n\", " ^ string_of_c_expr lhs ^ "-" ^ string_of_c_expr rhs)::[]
                                                                | Tvoid -> raise (Error("Invalid print type Void: " ^ string_of_c_expr lhs ^ "-" ^ string_of_c_expr rhs)))
                                            | _ -> raise (Error("Invalid add in function call"))
                                          )
                                  | Times -> (match lhs with
                                              C_Int(l) -> ("\"%d\\n\", " ^ string_of_c_expr lhs ^ "*" ^ string_of_c_expr rhs)::[]
                                            | C_Float(l) -> ("\"%f\\n\", " ^ string_of_c_expr lhs ^ "*" ^ string_of_c_expr rhs)::[]
                                            | C_Id(l, t) -> (match t with
                                                                  Tint -> ("\"%d\\n\", " ^ string_of_c_expr lhs ^ "*" ^ string_of_c_expr rhs)::[]
                                                                | Tfloat -> ("\"%f\\n\", " ^ string_of_c_expr lhs ^ "*" ^ string_of_c_expr rhs)::[]
                                                                | Tchar -> ("\"%c\\n\", " ^ string_of_c_expr lhs ^ "*" ^ string_of_c_expr rhs)::[]
                                                                | Tstring -> ("\"%s\\n\", " ^ string_of_c_expr lhs ^ "*" ^ string_of_c_expr rhs)::[]
                                                                | Tvoid -> raise (Error("Invalid print type Void: " ^ string_of_c_expr lhs ^ "*" ^ string_of_c_expr rhs)))
                                            | _ -> raise (Error("Invalid add in function call"))
                                          )
                                  | Divide -> (match lhs with
                                              C_Int(l) -> ("\"%d\\n\", " ^ string_of_c_expr lhs ^ "/" ^ string_of_c_expr rhs)::[]
                                            | C_Float(l) -> ("\"%f\\n\", " ^ string_of_c_expr lhs ^ "/" ^ string_of_c_expr rhs)::[]
                                            | C_Id(l, t) -> (match t with
                                                                  Tint -> ("\"%d\\n\", " ^ string_of_c_expr lhs ^ "/" ^ string_of_c_expr rhs)::[]
                                                                | Tfloat -> ("\"%f\\n\", " ^ string_of_c_expr lhs ^ "/" ^ string_of_c_expr rhs)::[]
                                                                | Tchar -> ("\"%c\\n\", " ^ string_of_c_expr lhs ^ "/" ^ string_of_c_expr rhs)::[]
                                                                | Tstring -> ("\"%s\\n\", " ^ string_of_c_expr lhs ^ "/" ^ string_of_c_expr rhs)::[]
                                                                | Tvoid -> raise (Error("Invalid print type Void: " ^ string_of_c_expr lhs ^ "/" ^ string_of_c_expr rhs)))
                                            | _ -> raise (Error("Invalid add in function call"))
                                          )
                                  | Equal -> ("\"%d\\n\", " ^ string_of_c_expr lhs ^ "==" ^ string_of_c_expr rhs)::[]
                                  | Ne -> ("\"%d\\n\", " ^ string_of_c_expr lhs ^ "!=" ^ string_of_c_expr rhs)::[]
                                  | Lt -> ("\"%d\\n\", " ^ string_of_c_expr lhs ^ "<" ^ string_of_c_expr rhs)::[]
                                  | Le -> ("\"%d\\n\", " ^ string_of_c_expr lhs ^ "<=" ^ string_of_c_expr rhs)::[]
                                  | Gt -> ("\"%d\\n\", " ^ string_of_c_expr lhs ^ ">" ^ string_of_c_expr rhs)::[]
                                  | Ge -> ("\"%d\\n\", " ^ string_of_c_expr lhs ^ ">=" ^ string_of_c_expr rhs)::[]
                                  | Or -> ("\"%d\\n\", " ^ string_of_c_expr lhs ^ "||" ^ string_of_c_expr rhs)::[]
                                  | And -> ("\"%d\\n\", " ^ string_of_c_expr lhs ^ "&&" ^ string_of_c_expr rhs)::[]
                                  | Mod -> ("\"%d\\n\", " ^ string_of_c_expr lhs ^ "%" ^ string_of_c_expr rhs)::[]
                                          )
      | _ -> raise (Error("Invalid expr in print statement: " ^ string_of_c_expr e))

      and error_2_fprintf (e: c_expr) = match e with
        C_Int(l) -> ("stderr, \"%d\\n\", " ^ string_of_c_expr e)::[]
      | C_Float(l) -> ("stderr, \"%f\\n\", " ^ string_of_c_expr e)::[]
      | C_Char(l) -> ("stderr, \"%c\\n\", " ^ string_of_c_expr e)::[]
      | C_String(l) -> ("stderr, \"%s\\n\", " ^ string_of_c_expr e)::[]
      | C_Id(l, t) -> (match t with
                        Tint -> ("stderr, \"%d\\n\", " ^ string_of_c_expr e)::[]
                        | Tfloat -> ("stderr, \"%f\\n\", " ^ string_of_c_expr e)::[]
                        | Tchar -> ("stderr, \"%c\\n\", " ^ string_of_c_expr e)::[]
                        | Tstring -> ("stderr, \"%s\\n\", " ^ string_of_c_expr e)::[]
                        | Tvoid -> raise (Error("Invalid print type Void: " ^ string_of_c_expr e)))
      | C_Binop(lhs, o, rhs) -> (match o with
                                    Add -> (match lhs with
                                              C_Int(l) -> ("stderr, \"%d\\n\", " ^ string_of_c_expr lhs ^ "+" ^ string_of_c_expr rhs)::[]
                                            | C_Float(l) -> ("stderr, \"%f\\n\", " ^ string_of_c_expr lhs ^ "+" ^ string_of_c_expr rhs)::[]
                                            | C_Id(l, t) -> (match t with
                                                                  Tint -> ("stderr, \"%d\\n\", " ^ string_of_c_expr lhs ^ "+" ^ string_of_c_expr rhs)::[]
                                                                | Tfloat -> ("stderr, \"%f\\n\", " ^ string_of_c_expr lhs ^ "+" ^ string_of_c_expr rhs)::[]
                                                                | Tchar -> ("stderr, \"%c\\n\", " ^ string_of_c_expr lhs ^ "+" ^ string_of_c_expr rhs)::[]
                                                                | Tstring -> ("stderr, \"%s\\n\", " ^ string_of_c_expr lhs ^ "+" ^ string_of_c_expr rhs)::[]
                                                                | Tvoid -> raise (Error("Invalid print type Void: " ^ string_of_c_expr lhs ^ "+" ^ string_of_c_expr rhs)))
                                            | _ -> raise (Error("Invalid add in function call"))
                                          )
                                  | Subtract -> (match lhs with
                                              C_Int(l) -> ("stderr, \"%d\\n\", " ^ string_of_c_expr lhs ^ "-" ^ string_of_c_expr rhs)::[]
                                            | C_Float(l) -> ("stderr, \"%f\\n\", " ^ string_of_c_expr lhs ^ "-" ^ string_of_c_expr rhs)::[]
                                            | C_Id(l, t) -> (match t with
                                                                  Tint -> ("stderr, \"%d\\n\", " ^ string_of_c_expr lhs ^ "-" ^ string_of_c_expr rhs)::[]
                                                                | Tfloat -> ("stderr, \"%f\\n\", " ^ string_of_c_expr lhs ^ "-" ^ string_of_c_expr rhs)::[]
                                                                | Tchar -> ("stderr, \"%c\\n\", " ^ string_of_c_expr lhs ^ "-" ^ string_of_c_expr rhs)::[]
                                                                | Tstring -> ("stderr, \"%s\\n\", " ^ string_of_c_expr lhs ^ "-" ^ string_of_c_expr rhs)::[]
                                                                | Tvoid -> raise (Error("Invalid print type Void: " ^ string_of_c_expr lhs ^ "-" ^ string_of_c_expr rhs)))
                                            | _ -> raise (Error("Invalid add in function call"))
                                          )
                                  | Times -> (match lhs with
                                              C_Int(l) -> ("stderr, \"%d\\n\", " ^ string_of_c_expr lhs ^ "*" ^ string_of_c_expr rhs)::[]
                                            | C_Float(l) -> ("stderr, stderr, \"%f\\n\", " ^ string_of_c_expr lhs ^ "*" ^ string_of_c_expr rhs)::[]
                                            | C_Id(l, t) -> (match t with
                                                                  Tint -> ("stderr, \"%d\\n\", " ^ string_of_c_expr lhs ^ "*" ^ string_of_c_expr rhs)::[]
                                                                | Tfloat -> ("stderr, \"%f\\n\", " ^ string_of_c_expr lhs ^ "*" ^ string_of_c_expr rhs)::[]
                                                                | Tchar -> ("stderr, \"%c\\n\", " ^ string_of_c_expr lhs ^ "*" ^ string_of_c_expr rhs)::[]
                                                                | Tstring -> ("stderr, \"%s\\n\", " ^ string_of_c_expr lhs ^ "*" ^ string_of_c_expr rhs)::[]
                                                                | Tvoid -> raise (Error("Invalid print type Void: " ^ string_of_c_expr lhs ^ "*" ^ string_of_c_expr rhs)))
                                            | _ -> raise (Error("Invalid add in function call"))
                                          )
                                  | Divide -> (match lhs with
                                              C_Int(l) -> ("stderr, \"%d\\n\", " ^ string_of_c_expr lhs ^ "/" ^ string_of_c_expr rhs)::[]
                                            | C_Float(l) -> ("stderr, \"%f\\n\", " ^ string_of_c_expr lhs ^ "/" ^ string_of_c_expr rhs)::[]
                                            | C_Id(l, t) -> (match t with
                                                                  Tint -> ("stderr, \"%d\\n\", " ^ string_of_c_expr lhs ^ "/" ^ string_of_c_expr rhs)::[]
                                                                | Tfloat -> ("stderr, \"%f\\n\", " ^ string_of_c_expr lhs ^ "/" ^ string_of_c_expr rhs)::[]
                                                                | Tchar -> ("stderr, \"%c\\n\", " ^ string_of_c_expr lhs ^ "/" ^ string_of_c_expr rhs)::[]
                                                                | Tstring -> ("stderr, \"%s\\n\", " ^ string_of_c_expr lhs ^ "/" ^ string_of_c_expr rhs)::[]
                                                                | Tvoid -> raise (Error("Invalid print type Void: " ^ string_of_c_expr lhs ^ "/" ^ string_of_c_expr rhs)))
                                            | _ -> raise (Error("Invalid add in function call"))
                                          )
                                  | Equal -> ("stderr, \"%d\\n\", " ^ string_of_c_expr lhs ^ "==" ^ string_of_c_expr rhs)::[]
                                  | Ne -> ("stderr, \"%d\\n\", " ^ string_of_c_expr lhs ^ "!=" ^ string_of_c_expr rhs)::[]
                                  | Lt -> ("stderr, \"%d\\n\", " ^ string_of_c_expr lhs ^ "<" ^ string_of_c_expr rhs)::[]
                                  | Le -> ("stderr, \"%d\\n\", " ^ string_of_c_expr lhs ^ "<=" ^ string_of_c_expr rhs)::[]
                                  | Gt -> ("stderr, \"%d\\n\", " ^ string_of_c_expr lhs ^ ">" ^ string_of_c_expr rhs)::[]
                                  | Ge -> ("stderr, \"%d\\n\", " ^ string_of_c_expr lhs ^ ">=" ^ string_of_c_expr rhs)::[]
                                  | Or -> ("stderr, \"%d\\n\", " ^ string_of_c_expr lhs ^ "||" ^ string_of_c_expr rhs)::[]
                                  | And -> ("stderr, \"%d\\n\", " ^ string_of_c_expr lhs ^ "&&" ^ string_of_c_expr rhs)::[]
                                  | Mod -> ("stderr, \"%d\\n\", " ^ string_of_c_expr lhs ^ "%" ^ string_of_c_expr rhs)::[]
                                          )
      | _ -> raise (Error("Invalid expr in print statement: " ^ string_of_c_expr e))

let string_of_c_vdecl vdecl = string_of_c_dataType vdecl.vdecl_type ^ " " ^ vdecl.vdecl_name (* " " ^ vdecl.array_size ^ *)

let string_of_c_arraydecl arraydecl = string_of_c_dataType arraydecl.arraydecl_type ^ " " ^ arraydecl.arraydecl_name ^ "[" ^
    string_of_expr arraydecl.arraydecl_size ^ "]"

let rec string_of_c_matrixlist (seed: string) el = match el with
    [] -> seed ^ "}"
    | head::tail -> string_of_c_matrixlist (seed ^ string_of_arraylist head ^ ",\n") tail

let string_of_c_matrixdecl m = string_of_c_dataType m.matrixdecl_type ^ " " ^ m.matrixdecl_name ^ "[" ^
    string_of_expr m.matrixdecl_rows ^ "][" ^ string_of_expr m.matrixdecl_cols ^ "]"

let convert_stitch_2_for var start s_end stride fname =
  let size = string_of_c_expr s_end in
  let threads = "\npthread_t *threadpool = malloc(NUMTHREADS * sizeof(pthread_t));\n" in 

  let thread_assignment = "info[thread].begin = i;\n" ^
                                  "if((i + 2*(" ^ size ^ "/NUMTHREADS)) > " ^ size ^ ") {\n" ^
                                  "info[thread].end = " ^ size ^ ";\n" ^
                                  "i = " ^ size ^ ";\n" ^
                                  "}\n" ^
                                  "else {\n" ^
                                  "info[thread].end = i + " ^ size ^ "/NUMTHREADS;\n" ^
                                  "}\n" in 

  let threadgen = "int e = pthread_create(&threadpool[thread], NULL, " ^ fname ^ ", &info[thread]);\n" ^
                  "if (e != 0) {\n" ^
                  "perror(\"Cannot create thread!\");\n" ^
                  "free(threadpool); //error, free the threadpool\n" ^
                  "exit(1);\n" ^
                  "}\n" in

  let varinfo = "struct stch_rangeInfo *info = malloc(sizeof(struct stch_rangeInfo) * NUMTHREADS);\n" in
  let incr = string_of_c_expr s_end ^ "/" ^ "NUMTHREADS" in
  let loop = threads ^ varinfo ^ "int thread = 0;\n" ^ "for(" in 
  loop ^ string_of_c_expr var ^ " = " ^ string_of_c_expr start ^ ";" ^ string_of_c_expr var ^ " < " ^
    string_of_c_expr s_end ^ ";" ^ string_of_c_expr var ^ " = " ^ string_of_c_expr var ^ "+" ^ incr ^ 
    ") {\n" ^ thread_assignment ^ threadgen ^ "thread++;\n" ^ "}\n\n"

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
  | C_Stitch(var, start, s_end, stride, fname, body) -> convert_stitch_2_for var start s_end stride fname
(*       "stitch " ^ string_of_c_expr s.stitchdecl_var ^
       " from " ^ string_of_c_expr s.stitchdecl_from ^
        " to " ^ string_of_c_expr s.stitchdecl_to ^ 
        " by " ^ string_of_c_expr s.stitchdecl_by ^ 
        " : " ^ s.stitchdecl_func ^
        "\n" *)
  | C_Assign(v, e) -> string_of_c_vdecl v ^ " = " ^ string_of_c_expr e ^ ";\n"
  | C_ArrayDecl(a) -> string_of_c_arraydecl a ^ ";\n"
  | C_ArrayInit(arraydecl, el) -> string_of_c_arraydecl arraydecl ^ " = {" ^ String.concat ", " (List.map string_of_expr el) ^ "};\n"
  | C_MatrixDecl(m) -> string_of_c_matrixdecl m ^ ";\n"
  | C_MatrixInit(mdecl, li) -> string_of_c_matrixdecl mdecl ^ " = " ^ string_of_c_matrixlist "{" li ^ ";\n"
  | C_Break -> "break;"

let rec stitch2func = function
    C_Stitch(var, start, s_end, stride, fname, body) -> "void *" ^ fname ^ " (void *vars)" ^ 
      String.concat "\n" (List.map string_of_c_stmt body) ^ "\n"
  | _ -> ""

let string_of_stitch func = String.concat "" (List.map stitch2func func.body) 

let string_of_c_fdecl fdecl =
  string_of_c_dataType fdecl.fdecl_type ^ " " ^ fdecl.fdecl_name ^ "(" ^ 
    String.concat ", " (List.map string_of_c_vdecl fdecl.fdecl_formals) ^ ")\n{\n" ^
    String.concat "" (List.map string_of_c_stmt fdecl.body) ^ "}\n"

let string_of_vars (_, s, _) = s

let string_of_c_program (prog : Stch_cast.c_program ) =
  String.concat "" (List.map string_of_c_stmt prog.stmts) ^ "\n" ^
  String.concat "\n" (List.map string_of_stitch prog.funcs) ^ "\n" ^ 
  String.concat "\n" (List.map string_of_c_fdecl prog.funcs) ^ "\n"



