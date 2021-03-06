(* 
C Code Generator
December 2015
Authors: Dan Cole & Tim Waterman

Takes the C_AST and Generates the corresponding C Code
*)

open Stch_ast
open Stch_cast
exception Error of string

let string_of_c_dataType = function
    Tint -> "int"
  | Tfloat -> "float"
  | Tchar -> "char"
  | Tvoid -> "void"
  | Tstring -> "char *"
  | Tintap -> "int"
  | Tintam -> "int"
  | Tfloatap -> "float"
  | Tfloatam -> "float"
  | Tfile -> "FILE *"

(* Generates the c code for the corresponding expression from our C_AST *)
let rec string_of_c_expr = function
    C_Int(l) -> string_of_int l
  | C_Float(l) -> string_of_float l
  | C_Char(l) ->  "\'" ^ String.make 1 l ^ "\'"
  | C_Escape(l) -> "\'" ^ l ^ "\'"
  | C_Id(s, t) -> s
  | C_String(s) -> "\"" ^ s ^ "\""
  | C_Binop(e1, o, e2) ->
      "(" ^ string_of_c_expr e1 ^ " " ^
      (match o with
	      Add -> "+" | Subtract -> "-" | Times -> "*" | Divide -> "/"
      | Equal -> "==" | Ne -> "!="
      | Lt -> "<" | Le -> "<=" | Gt -> ">" | Ge -> ">="
      | Or -> "||" | And -> "&&" | Mod -> "%" ) ^ " " ^
      string_of_c_expr e2 ^ ")"
  | C_Negate(e) -> "!" ^ string_of_c_expr e

  (* For call, we need to match our various built-in functions *)
  | C_Call(f, el) -> (match f with "print" -> "printf" 
                                  | "error" -> "fprintf" 
                                  | "open_r" -> "fopen"
                                  | "open_w" -> "fopen"
                                  | "read" -> "fread"
                                  | "write" -> "fwrite"
                                  | _ -> f) ^ 
                    "(" ^ String.concat ", " (match f with "print" -> print_2_fprint (List.hd el) 
                                                          | "error" -> error_2_fprintf (List.hd el) 
                                                          | "open_r" -> open_2_fopen_r (List.hd el) 
                                                          | "open_w" -> open_2_fopen_w (List.hd el)
                                                          | "read" -> read_2_fread el
                                                          | "write" -> write_2_fwrite el
                                                          | _ -> List.map string_of_c_expr el) ^ ")"
  | C_Assign2(i, e) -> i ^ " = " ^ string_of_c_expr e
  | C_Array_Item_Assign(id, ind, e) -> id ^ "[" ^ string_of_c_expr ind ^"] = " ^ string_of_c_expr e
  | C_Array_Index(a, i, t) -> a ^ "[" ^ string_of_c_expr i ^ "]"
  | C_Matrix_Index(m, r, c, t) -> m ^ "[" ^ string_of_c_expr r ^ "][" ^ string_of_c_expr c ^ "]"
  | C_Matrix_Item_Assign(m, r, c, e) -> m ^ "[" ^ string_of_c_expr r ^ "][" ^ string_of_c_expr c ^ "] = " ^ string_of_c_expr e
  | C_Noexpr -> ""

    (* Converting from read to the C function fread() *)
      and read_2_fread (el: c_expr list) = 
        let file = List.hd el in
          let arr = List.hd (List.rev el) in
            match file with
              C_Id(s, t) -> (match t with
                Tfile -> (match arr with
                  C_Id(s', t') -> (s' ^ ", sizeof(" ^ s'  ^ "), sizeof(" ^ string_of_c_dataType t' ^ "), " ^ s)::[]
                  | _ -> raise(Error("Invalid argument type for read: " ^ string_of_c_expr arr)))
                | _ -> raise(Error("Invalid argument type for read: " ^ string_of_c_expr file)))
              | _ -> raise(Error("Invalid argument for read: " ^ string_of_c_expr file))

    (* Converting for write to the C function fwrite() *)
      and write_2_fwrite (el: c_expr list) = 
        let file = List.hd el in
          let arr = List.hd (List.rev el) in
            match file with
              C_Id(s, t) -> (match t with
                Tfile -> (match arr with
                  C_Id(s', t') -> (s' ^ ", sizeof(" ^ s'  ^ "), sizeof(" ^ string_of_c_dataType t' ^ "), " ^ s)::[]
                  | _ -> raise(Error("Invalid argument type for read: " ^ string_of_c_expr arr)))
                | _ -> raise(Error("Invalid argument type for read: " ^ string_of_c_expr file)))
              | _ -> raise(Error("Invalid argument for read: " ^ string_of_c_expr file))

    (* Converting the two open functions *)
      and open_2_fopen_r (e: c_expr) = match e with
        C_String(l) -> ("\"" ^ l ^ "\", \"r+\"" )::[]
      | _ -> raise (Error("Invalid argument for open: " ^ string_of_c_expr e))

      and open_2_fopen_w (e: c_expr) = match e with
        C_String(l) -> ("\"" ^ l ^ "\", \"w+\"" )::[]
      | _ -> raise (Error("Invalid argument for open: " ^ string_of_c_expr e))

    (* Generating print statements based on args *)
      and print_2_fprint (e: c_expr) = match e with
        C_Int(l) -> ("\"%d\\n\", " ^ string_of_c_expr e)::[]
      | C_Float(l) -> ("\"%f\\n\", " ^ string_of_c_expr e)::[]
      | C_Char(l) -> ("\"%c\\n\", " ^ string_of_c_expr e)::[]
      | C_String(l) -> ("\"%s\\n\", " ^ string_of_c_expr e)::[]
      | C_Array_Index(a, i, t) -> (match t with
                                      Tint | Tintap | Tintam -> ("\"%d\\n\", " ^ a ^ "[" ^ string_of_c_expr i ^ "]")::[]
                                    | Tfloat | Tfloatap | Tfloatam -> ("\"%f\\n\", " ^ a ^ "[" ^ string_of_c_expr i ^ "]")::[]
                                    | Tchar -> ("\"%c\\n\", " ^ a ^ "[" ^ string_of_c_expr i ^ "]")::[]
                                    | Tstring -> ("\"%s\\n\", " ^ a ^ "[" ^ string_of_c_expr i ^ "]")::[]
                                    | Tvoid -> raise (Error("Invalid print type Void: " ^ a ^ "[" ^ string_of_c_expr i ^ "]"))
                                    | Tfile -> raise (Error("Invalid print type File")))
      | C_Matrix_Index(m, r, c, t) -> (match t with
                                     Tint | Tintap | Tintam -> ("\"%d\\n\", " ^ m ^ "[" ^ string_of_c_expr r ^ "][" ^ 
                                              string_of_c_expr c ^ "]")::[]
                                    | Tfloat | Tfloatap | Tfloatam -> ("\"%f\\n\", " ^ m ^ "[" ^ string_of_c_expr r ^ "][" ^
                                              string_of_c_expr c ^ "]")::[]
                                    | Tchar -> ("\"%c\\n\", " ^ m ^ "[" ^ string_of_c_expr r ^ "][" ^
                                              string_of_c_expr c ^ "]")::[]
                                    | Tstring -> ("\"%s\\n\", " ^ m ^ "[" ^ string_of_c_expr r ^ "][" ^
                                              string_of_c_expr c ^ "]")::[]
                                    | Tvoid -> raise(Error("Invalid print type void in matrix printing"))
                                    | Tfile -> raise(Error("Invlaid print type file in matrix printing")))
      | C_Id(l, t) -> (match t with
                        Tint | Tintap | Tintam -> ("\"%d\\n\", " ^ string_of_c_expr e)::[]
                        | Tfloat | Tfloatap | Tfloatam -> ("\"%f\\n\", " ^ string_of_c_expr e)::[]
                        | Tchar -> ("\"%c\\n\", " ^ string_of_c_expr e)::[]
                        | Tstring -> ("\"%s\\n\", " ^ string_of_c_expr e)::[]
                        | Tvoid -> raise (Error("Invalid print type Void: " ^ string_of_c_expr e))
                        | Tfile -> raise (Error("Invalid print type File: ")))
      | C_Binop(lhs, o, rhs) -> (match o with
                                    Add -> (match lhs with
                                              C_Int(l) -> ("\"%d\\n\", " ^ string_of_c_expr lhs ^ "+" ^ string_of_c_expr rhs)::[]
                                            | C_Float(l) -> ("\"%f\\n\", " ^ string_of_c_expr lhs ^ "+" ^ string_of_c_expr rhs)::[]
                                            | C_Id(l, t) -> (match t with
                                                                  Tint | Tintap | Tintam -> ("\"%d\\n\", " ^ string_of_c_expr lhs ^ "+" ^ string_of_c_expr rhs)::[]
                                                                | Tfloat | Tfloatap | Tfloatam -> ("\"%f\\n\", " ^ string_of_c_expr lhs ^ "+" ^ string_of_c_expr rhs)::[]
                                                                | Tchar -> ("\"%c\\n\", " ^ string_of_c_expr lhs ^ "+" ^ string_of_c_expr rhs)::[]
                                                                | Tstring -> ("\"%s\\n\", " ^ string_of_c_expr lhs ^ "+" ^ string_of_c_expr rhs)::[]
                                                                | Tvoid -> raise (Error("Invalid print type Void: " ^ string_of_c_expr lhs ^ "+" ^ string_of_c_expr rhs))
                                                                | Tfile -> raise (Error("Invalid print type File: ")))
                                            | _ -> raise (Error("Invalid add in function call"))
                                          )
                                  | Subtract -> (match lhs with
                                              C_Int(l) -> ("\"%d\\n\", " ^ string_of_c_expr lhs ^ "-" ^ string_of_c_expr rhs)::[]
                                            | C_Float(l) -> ("\"%f\\n\", " ^ string_of_c_expr lhs ^ "-" ^ string_of_c_expr rhs)::[]
                                            | C_Id(l, t) -> (match t with
                                                                  Tint | Tintap | Tintam -> ("\"%d\\n\", " ^ string_of_c_expr lhs ^ "-" ^ string_of_c_expr rhs)::[]
                                                                | Tfloat | Tfloatap | Tfloatam -> ("\"%f\\n\", " ^ string_of_c_expr lhs ^ "-" ^ string_of_c_expr rhs)::[]
                                                                | Tchar -> ("\"%c\\n\", " ^ string_of_c_expr lhs ^ "-" ^ string_of_c_expr rhs)::[]
                                                                | Tstring -> ("\"%s\\n\", " ^ string_of_c_expr lhs ^ "-" ^ string_of_c_expr rhs)::[]
                                                                | Tvoid -> raise (Error("Invalid print type Void: " ^ string_of_c_expr lhs ^ "-" ^ string_of_c_expr rhs))
                                                                | Tfile -> raise (Error("Invalid print type File: ")))
                                            | _ -> raise (Error("Invalid add in function call"))
                                          )
                                  | Times -> (match lhs with
                                              C_Int(l) -> ("\"%d\\n\", " ^ string_of_c_expr lhs ^ "*" ^ string_of_c_expr rhs)::[]
                                            | C_Float(l) -> ("\"%f\\n\", " ^ string_of_c_expr lhs ^ "*" ^ string_of_c_expr rhs)::[]
                                            | C_Id(l, t) -> (match t with
                                                                  Tint | Tintap | Tintam -> ("\"%d\\n\", " ^ string_of_c_expr lhs ^ "*" ^ string_of_c_expr rhs)::[]
                                                                | Tfloat | Tfloatap | Tfloatam -> ("\"%f\\n\", " ^ string_of_c_expr lhs ^ "*" ^ string_of_c_expr rhs)::[]
                                                                | Tchar -> ("\"%c\\n\", " ^ string_of_c_expr lhs ^ "*" ^ string_of_c_expr rhs)::[]
                                                                | Tstring -> ("\"%s\\n\", " ^ string_of_c_expr lhs ^ "*" ^ string_of_c_expr rhs)::[]
                                                                | Tvoid -> raise (Error("Invalid print type Void: " ^ string_of_c_expr lhs ^ "*" ^ string_of_c_expr rhs))
                                                                | Tfile -> raise (Error("Invalid print type File: ")))
                                            | _ -> raise (Error("Invalid add in function call"))
                                          )
                                  | Divide -> (match lhs with
                                              C_Int(l) -> ("\"%d\\n\", " ^ string_of_c_expr lhs ^ "/" ^ string_of_c_expr rhs)::[]
                                            | C_Float(l) -> ("\"%f\\n\", " ^ string_of_c_expr lhs ^ "/" ^ string_of_c_expr rhs)::[]
                                            | C_Id(l, t) -> (match t with
                                                                  Tint | Tintap | Tintam -> ("\"%d\\n\", " ^ string_of_c_expr lhs ^ "/" ^ string_of_c_expr rhs)::[]
                                                                | Tfloat | Tfloatap | Tfloatam -> ("\"%f\\n\", " ^ string_of_c_expr lhs ^ "/" ^ string_of_c_expr rhs)::[]
                                                                | Tchar -> ("\"%c\\n\", " ^ string_of_c_expr lhs ^ "/" ^ string_of_c_expr rhs)::[]
                                                                | Tstring -> ("\"%s\\n\", " ^ string_of_c_expr lhs ^ "/" ^ string_of_c_expr rhs)::[]
                                                                | Tvoid -> raise (Error("Invalid print type Void: " ^ string_of_c_expr lhs ^ "/" ^ string_of_c_expr rhs))
                                                                | Tfile -> raise (Error("Invalid print type File: ")))
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

    (* Generating the error function based on parameters *)
      and error_2_fprintf (e: c_expr) = match e with
        C_Int(l) -> ("stderr, \"%d\\n\", " ^ string_of_c_expr e)::[]
      | C_Float(l) -> ("stderr, \"%f\\n\", " ^ string_of_c_expr e)::[]
      | C_Char(l) -> ("stderr, \"%c\\n\", " ^ string_of_c_expr e)::[]
      | C_String(l) -> ("stderr, \"%s\\n\", " ^ string_of_c_expr e)::[]
      | C_Id(l, t) -> (match t with
                        Tint | Tintap | Tintam -> ("stderr, \"%d\\n\", " ^ string_of_c_expr e)::[]
                        | Tfloat | Tfloatap | Tfloatam -> ("stderr, \"%f\\n\", " ^ string_of_c_expr e)::[]
                        | Tchar -> ("stderr, \"%c\\n\", " ^ string_of_c_expr e)::[]
                        | Tstring -> ("stderr, \"%s\\n\", " ^ string_of_c_expr e)::[]
                        | Tvoid -> raise (Error("Invalid print type Void: " ^ string_of_c_expr e))
                        | Tfile -> raise (Error("Invalid print type File: ")))
      | C_Binop(lhs, o, rhs) -> (match o with
                                    Add -> (match lhs with
                                              C_Int(l) -> ("stderr, \"%d\\n\", " ^ string_of_c_expr lhs ^ "+" ^ string_of_c_expr rhs)::[]
                                            | C_Float(l) -> ("stderr, \"%f\\n\", " ^ string_of_c_expr lhs ^ "+" ^ string_of_c_expr rhs)::[]
                                            | C_Id(l, t) -> (match t with
                                                                  Tint | Tintap | Tintam -> ("stderr, \"%d\\n\", " ^ string_of_c_expr lhs ^ "+" ^ string_of_c_expr rhs)::[]
                                                                | Tfloat | Tfloatap | Tfloatam -> ("stderr, \"%f\\n\", " ^ string_of_c_expr lhs ^ "+" ^ string_of_c_expr rhs)::[]
                                                                | Tchar -> ("stderr, \"%c\\n\", " ^ string_of_c_expr lhs ^ "+" ^ string_of_c_expr rhs)::[]
                                                                | Tstring -> ("stderr, \"%s\\n\", " ^ string_of_c_expr lhs ^ "+" ^ string_of_c_expr rhs)::[]
                                                                | Tvoid -> raise (Error("Invalid print type Void: " ^ string_of_c_expr lhs ^ "+" ^ string_of_c_expr rhs))
                                                                | Tfile -> raise (Error("Invalid print type File: ")))
                                            | _ -> raise (Error("Invalid add in function call"))
                                          )
                                  | Subtract -> (match lhs with
                                              C_Int(l) -> ("stderr, \"%d\\n\", " ^ string_of_c_expr lhs ^ "-" ^ string_of_c_expr rhs)::[]
                                            | C_Float(l) -> ("stderr, \"%f\\n\", " ^ string_of_c_expr lhs ^ "-" ^ string_of_c_expr rhs)::[]
                                            | C_Id(l, t) -> (match t with
                                                                  Tint | Tintap | Tintam -> ("stderr, \"%d\\n\", " ^ string_of_c_expr lhs ^ "-" ^ string_of_c_expr rhs)::[]
                                                                | Tfloat | Tfloatap | Tfloatam -> ("stderr, \"%f\\n\", " ^ string_of_c_expr lhs ^ "-" ^ string_of_c_expr rhs)::[]
                                                                | Tchar -> ("stderr, \"%c\\n\", " ^ string_of_c_expr lhs ^ "-" ^ string_of_c_expr rhs)::[]
                                                                | Tstring -> ("stderr, \"%s\\n\", " ^ string_of_c_expr lhs ^ "-" ^ string_of_c_expr rhs)::[]
                                                                | Tvoid -> raise (Error("Invalid print type Void: " ^ string_of_c_expr lhs ^ "-" ^ string_of_c_expr rhs))
                                                                | Tfile -> raise (Error("Invalid print type File: ")))
                                            | _ -> raise (Error("Invalid add in function call"))
                                          )
                                  | Times -> (match lhs with
                                              C_Int(l) -> ("stderr, \"%d\\n\", " ^ string_of_c_expr lhs ^ "*" ^ string_of_c_expr rhs)::[]
                                            | C_Float(l) -> ("stderr, stderr, \"%f\\n\", " ^ string_of_c_expr lhs ^ "*" ^ string_of_c_expr rhs)::[]
                                            | C_Id(l, t) -> (match t with
                                                                  Tint | Tintap | Tintam -> ("stderr, \"%d\\n\", " ^ string_of_c_expr lhs ^ "*" ^ string_of_c_expr rhs)::[]
                                                                | Tfloat | Tfloatap | Tfloatam -> ("stderr, \"%f\\n\", " ^ string_of_c_expr lhs ^ "*" ^ string_of_c_expr rhs)::[]
                                                                | Tchar -> ("stderr, \"%c\\n\", " ^ string_of_c_expr lhs ^ "*" ^ string_of_c_expr rhs)::[]
                                                                | Tstring -> ("stderr, \"%s\\n\", " ^ string_of_c_expr lhs ^ "*" ^ string_of_c_expr rhs)::[]
                                                                | Tvoid -> raise (Error("Invalid print type Void: " ^ string_of_c_expr lhs ^ "*" ^ string_of_c_expr rhs))
                                                                | Tfile -> raise (Error("Invalid print type File: ")))
                                            | _ -> raise (Error("Invalid add in function call"))
                                          )
                                  | Divide -> (match lhs with
                                              C_Int(l) -> ("stderr, \"%d\\n\", " ^ string_of_c_expr lhs ^ "/" ^ string_of_c_expr rhs)::[]
                                            | C_Float(l) -> ("stderr, \"%f\\n\", " ^ string_of_c_expr lhs ^ "/" ^ string_of_c_expr rhs)::[]
                                            | C_Id(l, t) -> (match t with
                                                                  Tint | Tintap | Tintam -> ("stderr, \"%d\\n\", " ^ string_of_c_expr lhs ^ "/" ^ string_of_c_expr rhs)::[]
                                                                | Tfloat | Tfloatap | Tfloatam -> ("stderr, \"%f\\n\", " ^ string_of_c_expr lhs ^ "/" ^ string_of_c_expr rhs)::[]
                                                                | Tchar -> ("stderr, \"%c\\n\", " ^ string_of_c_expr lhs ^ "/" ^ string_of_c_expr rhs)::[]
                                                                | Tstring -> ("stderr, \"%s\\n\", " ^ string_of_c_expr lhs ^ "/" ^ string_of_c_expr rhs)::[]
                                                                | Tvoid -> raise (Error("Invalid print type Void: " ^ string_of_c_expr lhs ^ "/" ^ string_of_c_expr rhs))
                                                                | Tfile -> raise (Error("Invalid print type File: ")))
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

(* 
String of stitch expression is for use exclusively inside stitch loops.
It takes in a structname and a symbol table in addition to the corresponding expression.
The structname is used to prepend onto variables that exist inside the symtable (which means
  that the variables are passed into the function externally) 
*)
let rec string_of_stch_expr (structname: string) (table: symTable) (exp: c_expr) = match exp with
    C_Int(l) -> string_of_int l
  | C_Float(l) -> string_of_float l
  | C_Char(l) ->  "\'" ^ String.make 1 l ^ "\'"
  | C_Id(s, t) -> (* structname ^ "->" ^ s *)
      if List.exists( fun(_,n,_) -> n = s) table.vars then
          structname ^ "->" ^ s
      else
          s      
  | C_Escape(l) -> "\'" ^ l ^ "\'"
  | C_String(s) -> "\"" ^ s ^ "\""
  | C_Binop(e1, o, e2) ->
      (string_of_stch_expr structname table e1) ^ " " ^
      (match o with
        Add -> "+" | Subtract -> "-" | Times -> "*" | Divide -> "/"
      | Equal -> "==" | Ne -> "!="
      | Lt -> "<" | Le -> "<=" | Gt -> ">" | Ge -> ">="
      | Or -> "||" | And -> "&&" | Mod -> "%" ) ^ " " ^
      (string_of_stch_expr structname table e2)
  | C_Negate(e) -> "!" ^ string_of_stch_expr structname table e
  | C_Call(f, el) -> (match f with 
        "print" -> "printf"
        | "error" -> "fprintf"
        | _ -> f) ^ "(" ^ String.concat ", " (match f with 
          "print" -> print_2_fprint (List.hd el) structname table
          | "error" -> error_2_fprintf (List.hd el) | _ -> List.map string_of_c_expr el) ^ ")"
  (* Now we need to check to see if the id's are in the table  *)
  | C_Assign2(i, e) -> 
      if List.exists( fun(_,s,_) -> s = i) table.vars then
        structname ^ "->" ^ i ^ " = " ^ string_of_stch_expr structname table e
      else
        i ^ " = " ^ string_of_stch_expr structname table e
  | C_Array_Item_Assign(id, ind, e) -> 
      if List.exists( fun(_,s,_) -> s = id) table.vars then
        structname ^ "->" ^ id ^ "[" ^ string_of_stch_expr structname table ind ^
        "] = " ^ string_of_stch_expr structname table e
      else
        id ^ "[" ^ string_of_stch_expr structname table ind ^
        "] = " ^ string_of_stch_expr structname table e
  | C_Array_Index(a, i, t) -> 
      if List.exists( fun(_,s,_) -> s = a) table.vars then
        structname ^ "->" ^ a ^ "[" ^ string_of_stch_expr structname table i ^ "]"
      else
        a ^ "[" ^ string_of_stch_expr structname table i ^ "]"
  | C_Matrix_Index(m, r, c, t) -> 
      if List.exists( fun(_,s,_) -> s = m) table.vars then
        structname ^ "->" ^ m ^ "[" ^ string_of_stch_expr structname table r ^
        "][" ^ string_of_stch_expr structname table c ^ "]"
      else
        m ^ "[" ^ string_of_stch_expr structname table r ^
        "][" ^ string_of_stch_expr structname table c ^ "]"
  | C_Matrix_Item_Assign(m, r, c, e) -> 
      if List.exists( fun(_,s,_) -> s = m) table.vars then
        structname ^ "->" ^ m ^ "[" ^ string_of_stch_expr structname table r ^ 
        "][" ^ string_of_stch_expr structname table c ^ "] = " ^ string_of_stch_expr structname table e
      else 
        m ^ "[" ^ string_of_stch_expr structname table r ^ 
        "][" ^ string_of_stch_expr structname table c ^ "] = " ^ string_of_stch_expr structname table e
  | C_Noexpr -> ""

      and print_2_fprint (e: c_expr) (structname: string) (table: symTable) = match e with
        C_Int(l) -> ("\"%d\\n\", " ^ (string_of_stch_expr structname table e))::[]
      | C_Float(l) -> ("\"%f\\n\", " ^ string_of_c_expr e)::[]
      | C_Char(l) -> ("\"%c\\n\", " ^ string_of_c_expr e)::[]
      | C_String(l) -> ("\"%s\\n\", " ^ string_of_c_expr e)::[]
      | C_Array_Index(a, i, t) -> (match t with
                                      Tint | Tintap | Tintam -> ("\"%d\\n\", " ^ a ^ "[" ^ string_of_c_expr i ^ "]")::[]
                                    | Tfloat | Tfloatap | Tfloatam -> ("\"%f\\n\", " ^ a ^ "[" ^ string_of_c_expr i ^ "]")::[]
                                    | Tchar -> ("\"%c\\n\", " ^ a ^ "[" ^ string_of_c_expr i ^ "]")::[]
                                    | Tstring -> ("\"%s\\n\", " ^ a ^ "[" ^ string_of_c_expr i ^ "]")::[]
                                    | Tvoid -> raise (Error("Invalid print type Void: " ^ a ^ "[" ^ string_of_c_expr i ^ "]"))
                                    | Tfile -> raise (Error("Invalid print type File: ")))
      | C_Matrix_Index(m, r, c, t) -> (match t with
                                     Tint | Tintap | Tintam -> ("\"%d\\n\", " ^ m ^ "[" ^ string_of_c_expr r ^ "][" ^ 
                                              string_of_c_expr c ^ "]")::[]
                                    | Tfloat | Tfloatap | Tfloatam -> ("\"%f\\n\", " ^ m ^ "[" ^ string_of_c_expr r ^ "][" ^
                                              string_of_c_expr c ^ "]")::[]
                                    | Tchar -> ("\"%c\\n\", " ^ m ^ "[" ^ string_of_c_expr r ^ "][" ^
                                              string_of_c_expr c ^ "]")::[]
                                    | Tstring -> ("\"%s\\n\", " ^ m ^ "[" ^ string_of_c_expr r ^ "][" ^
                                              string_of_c_expr c ^ "]")::[]
                                    | Tvoid -> raise(Error("Invalid print type void in matrix printing"))
                                    | Tfile -> raise (Error("Invalid print type File: ")))
      | C_Id(l, t) -> (match t with
                          Tint | Tintap | Tintam -> ("\"%d\\n\", " ^ (string_of_stch_expr structname table e))::[]
                        | Tfloat | Tfloatap | Tfloatam -> ("\"%f\\n\", " ^ string_of_stch_expr structname table e)::[]
                        | Tchar -> ("\"%c\\n\", " ^ string_of_c_expr e)::[]
                        | Tstring -> ("\"%s\\n\", " ^ string_of_c_expr e)::[]
                        | Tvoid -> raise (Error("Invalid print type Void: " ^ string_of_c_expr e))
                        | Tfile -> raise (Error("Invalid print type File: ")))
      | C_Binop(lhs, o, rhs) -> (match o with
                                    Add -> (match lhs with
                                              C_Int(l) -> ("\"%d\\n\", " ^ string_of_c_expr lhs ^ "+" ^ string_of_c_expr rhs)::[]
                                            | C_Float(l) -> ("\"%f\\n\", " ^ string_of_c_expr lhs ^ "+" ^ string_of_c_expr rhs)::[]
                                            | C_Id(l, t) -> (match t with
                                                                  Tint | Tintap | Tintam -> ("\"%d\\n\", " ^ string_of_c_expr lhs ^ "+" ^ string_of_c_expr rhs)::[]
                                                                | Tfloat | Tfloatap | Tfloatam -> ("\"%f\\n\", " ^ string_of_c_expr lhs ^ "+" ^ string_of_c_expr rhs)::[]
                                                                | Tchar -> ("\"%c\\n\", " ^ string_of_c_expr lhs ^ "+" ^ string_of_c_expr rhs)::[]
                                                                | Tstring -> ("\"%s\\n\", " ^ string_of_c_expr lhs ^ "+" ^ string_of_c_expr rhs)::[]
                                                                | Tvoid -> raise (Error("Invalid print type Void: " ^ string_of_c_expr lhs ^ "+" ^ string_of_c_expr rhs))
                                                                | Tfile -> raise (Error("Invalid print type File: ")))
                                            | _ -> raise (Error("Invalid add in function call"))
                                          )
                                  | Subtract -> (match lhs with
                                              C_Int(l) -> ("\"%d\\n\", " ^ string_of_c_expr lhs ^ "-" ^ string_of_c_expr rhs)::[]
                                            | C_Float(l) -> ("\"%f\\n\", " ^ string_of_c_expr lhs ^ "-" ^ string_of_c_expr rhs)::[]
                                            | C_Id(l, t) -> (match t with
                                                                  Tint | Tintap | Tintam -> ("\"%d\\n\", " ^ string_of_c_expr lhs ^ "-" ^ string_of_c_expr rhs)::[]
                                                                | Tfloat | Tfloatap | Tfloatam -> ("\"%f\\n\", " ^ string_of_c_expr lhs ^ "-" ^ string_of_c_expr rhs)::[]
                                                                | Tchar -> ("\"%c\\n\", " ^ string_of_c_expr lhs ^ "-" ^ string_of_c_expr rhs)::[]
                                                                | Tstring -> ("\"%s\\n\", " ^ string_of_c_expr lhs ^ "-" ^ string_of_c_expr rhs)::[]
                                                                | Tvoid -> raise (Error("Invalid print type Void: " ^ string_of_c_expr lhs ^ "-" ^ string_of_c_expr rhs))
                                                                | Tfile -> raise (Error("Invalid print type File: ")))
                                            | _ -> raise (Error("Invalid add in function call"))
                                          )
                                  | Times -> (match lhs with
                                              C_Int(l) -> ("\"%d\\n\", " ^ string_of_c_expr lhs ^ "*" ^ string_of_c_expr rhs)::[]
                                            | C_Float(l) -> ("\"%f\\n\", " ^ string_of_c_expr lhs ^ "*" ^ string_of_c_expr rhs)::[]
                                            | C_Id(l, t) -> (match t with
                                                                  Tint | Tintap | Tintam -> ("\"%d\\n\", " ^ string_of_c_expr lhs ^ "*" ^ string_of_c_expr rhs)::[]
                                                                | Tfloat | Tfloatap | Tfloatam -> ("\"%f\\n\", " ^ string_of_c_expr lhs ^ "*" ^ string_of_c_expr rhs)::[]
                                                                | Tchar -> ("\"%c\\n\", " ^ string_of_c_expr lhs ^ "*" ^ string_of_c_expr rhs)::[]
                                                                | Tstring -> ("\"%s\\n\", " ^ string_of_c_expr lhs ^ "*" ^ string_of_c_expr rhs)::[]
                                                                | Tvoid -> raise (Error("Invalid print type Void: " ^ string_of_c_expr lhs ^ "*" ^ string_of_c_expr rhs))
                                                                | Tfile -> raise (Error("Invalid print type File: ")))
                                            | _ -> raise (Error("Invalid add in function call"))
                                          )
                                  | Divide -> (match lhs with
                                              C_Int(l) -> ("\"%d\\n\", " ^ string_of_c_expr lhs ^ "/" ^ string_of_c_expr rhs)::[]
                                            | C_Float(l) -> ("\"%f\\n\", " ^ string_of_c_expr lhs ^ "/" ^ string_of_c_expr rhs)::[]
                                            | C_Id(l, t) -> (match t with
                                                                  Tint | Tintap | Tintam -> ("\"%d\\n\", " ^ string_of_c_expr lhs ^ "/" ^ string_of_c_expr rhs)::[]
                                                                | Tfloat | Tfloatap | Tfloatam -> ("\"%f\\n\", " ^ string_of_c_expr lhs ^ "/" ^ string_of_c_expr rhs)::[]
                                                                | Tchar -> ("\"%c\\n\", " ^ string_of_c_expr lhs ^ "/" ^ string_of_c_expr rhs)::[]
                                                                | Tstring -> ("\"%s\\n\", " ^ string_of_c_expr lhs ^ "/" ^ string_of_c_expr rhs)::[]
                                                                | Tvoid -> raise (Error("Invalid print type Void: " ^ string_of_c_expr lhs ^ "/" ^ string_of_c_expr rhs))
                                                                | Tfile -> raise (Error("Invalid print type File: ")))
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
                          Tint | Tintap | Tintam -> ("stderr, \"%d\\n\", " ^ string_of_c_expr e)::[]
                        | Tfloat | Tfloatap | Tfloatam -> ("stderr, \"%f\\n\", " ^ string_of_c_expr e)::[]
                        | Tchar -> ("stderr, \"%c\\n\", " ^ string_of_c_expr e)::[]
                        | Tstring -> ("stderr, \"%s\\n\", " ^ string_of_c_expr e)::[]
                        | Tvoid -> raise (Error("Invalid print type Void: " ^ string_of_c_expr e))
                        | Tfile -> raise (Error("Invalid print type File: ")))
      | C_Binop(lhs, o, rhs) -> (match o with
                                    Add -> (match lhs with
                                              C_Int(l) -> ("stderr, \"%d\\n\", " ^ string_of_c_expr lhs ^ "+" ^ string_of_c_expr rhs)::[]
                                            | C_Float(l) -> ("stderr, \"%f\\n\", " ^ string_of_c_expr lhs ^ "+" ^ string_of_c_expr rhs)::[]
                                            | C_Id(l, t) -> (match t with
                                                                  Tint | Tintap | Tintam -> ("stderr, \"%d\\n\", " ^ string_of_c_expr lhs ^ "+" ^ string_of_c_expr rhs)::[]
                                                                | Tfloat | Tfloatap | Tfloatam -> ("stderr, \"%f\\n\", " ^ string_of_c_expr lhs ^ "+" ^ string_of_c_expr rhs)::[]
                                                                | Tchar -> ("stderr, \"%c\\n\", " ^ string_of_c_expr lhs ^ "+" ^ string_of_c_expr rhs)::[]
                                                                | Tstring -> ("stderr, \"%s\\n\", " ^ string_of_c_expr lhs ^ "+" ^ string_of_c_expr rhs)::[]
                                                                | Tvoid -> raise (Error("Invalid print type Void: " ^ string_of_c_expr lhs ^ "+" ^ string_of_c_expr rhs))
                                                                | Tfile -> raise (Error("Invalid print type File: ")))
                                            | _ -> raise (Error("Invalid add in function call"))
                                          )
                                  | Subtract -> (match lhs with
                                              C_Int(l) -> ("stderr, \"%d\\n\", " ^ string_of_c_expr lhs ^ "-" ^ string_of_c_expr rhs)::[]
                                            | C_Float(l) -> ("stderr, \"%f\\n\", " ^ string_of_c_expr lhs ^ "-" ^ string_of_c_expr rhs)::[]
                                            | C_Id(l, t) -> (match t with
                                                                  Tint | Tintap | Tintam -> ("stderr, \"%d\\n\", " ^ string_of_c_expr lhs ^ "-" ^ string_of_c_expr rhs)::[]
                                                                | Tfloat | Tfloatap | Tfloatam -> ("stderr, \"%f\\n\", " ^ string_of_c_expr lhs ^ "-" ^ string_of_c_expr rhs)::[]
                                                                | Tchar -> ("stderr, \"%c\\n\", " ^ string_of_c_expr lhs ^ "-" ^ string_of_c_expr rhs)::[]
                                                                | Tstring -> ("stderr, \"%s\\n\", " ^ string_of_c_expr lhs ^ "-" ^ string_of_c_expr rhs)::[]
                                                                | Tvoid -> raise (Error("Invalid print type Void: " ^ string_of_c_expr lhs ^ "-" ^ string_of_c_expr rhs))
                                                                | Tfile -> raise (Error("Invalid print type File: ")))
                                            | _ -> raise (Error("Invalid add in function call"))
                                          )
                                  | Times -> (match lhs with
                                              C_Int(l) -> ("stderr, \"%d\\n\", " ^ string_of_c_expr lhs ^ "*" ^ string_of_c_expr rhs)::[]
                                            | C_Float(l) -> ("stderr, stderr, \"%f\\n\", " ^ string_of_c_expr lhs ^ "*" ^ string_of_c_expr rhs)::[]
                                            | C_Id(l, t) -> (match t with
                                                                  Tint | Tintap | Tintam -> ("stderr, \"%d\\n\", " ^ string_of_c_expr lhs ^ "*" ^ string_of_c_expr rhs)::[]
                                                                | Tfloat | Tfloatap | Tfloatam -> ("stderr, \"%f\\n\", " ^ string_of_c_expr lhs ^ "*" ^ string_of_c_expr rhs)::[]
                                                                | Tchar -> ("stderr, \"%c\\n\", " ^ string_of_c_expr lhs ^ "*" ^ string_of_c_expr rhs)::[]
                                                                | Tstring -> ("stderr, \"%s\\n\", " ^ string_of_c_expr lhs ^ "*" ^ string_of_c_expr rhs)::[]
                                                                | Tvoid -> raise (Error("Invalid print type Void: " ^ string_of_c_expr lhs ^ "*" ^ string_of_c_expr rhs))
                                                                | Tfile -> raise (Error("Invalid print type File: ")))
                                            | _ -> raise (Error("Invalid add in function call"))
                                          )
                                  | Divide -> (match lhs with
                                              C_Int(l) -> ("stderr, \"%d\\n\", " ^ string_of_c_expr lhs ^ "/" ^ string_of_c_expr rhs)::[]
                                            | C_Float(l) -> ("stderr, \"%f\\n\", " ^ string_of_c_expr lhs ^ "/" ^ string_of_c_expr rhs)::[]
                                            | C_Id(l, t) -> (match t with
                                                                  Tint | Tintap | Tintam -> ("stderr, \"%d\\n\", " ^ string_of_c_expr lhs ^ "/" ^ string_of_c_expr rhs)::[]
                                                                | Tfloat | Tfloatap | Tfloatam -> ("stderr, \"%f\\n\", " ^ string_of_c_expr lhs ^ "/" ^ string_of_c_expr rhs)::[]
                                                                | Tchar -> ("stderr, \"%c\\n\", " ^ string_of_c_expr lhs ^ "/" ^ string_of_c_expr rhs)::[]
                                                                | Tstring -> ("stderr, \"%s\\n\", " ^ string_of_c_expr lhs ^ "/" ^ string_of_c_expr rhs)::[]
                                                                | Tvoid -> raise (Error("Invalid print type Void: " ^ string_of_c_expr lhs ^ "/" ^ string_of_c_expr rhs))
                                                                | Tfile -> raise (Error("Invalid print type File: ")))
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

(* print stitch variables will generate the C code to put the necessary variables
   into the struct that gets passed to the pthread function. Most of these are straightforward,
   just copying the name into the struct. For matrices and arrays, we need to generate special
   pointer notation since we aren't copying these like the other variables
    *)
let rec print_stitch_variables (seed: string) el = match el with
  [] -> seed ^ "\n"
  | head::tail -> let (typ, name, exp) = head in
    if exp = C_Noexpr then 
      print_stitch_variables (seed ^ (string_of_dataType typ) ^ " " ^ name ^ string_of_c_expr exp ^ ";\n") tail
    else (match exp with 
      | C_Matrix_Index(nm,ro,col,dt) -> print_stitch_variables (seed ^ (string_of_dataType typ) ^
        " (* " ^ name ^ ")["^string_of_c_expr col^"];\n") tail
      | C_Array_Index(name, exp,typ) -> print_stitch_variables (seed ^ 
        (string_of_dataType typ) ^ " *" ^ name ^ ";\n") tail
      | _ -> raise(Error("How did we even get here?")) )

(* Assign stitch variables is like print_stitch_variables, except it generates the C code to assign
  the local variables into their counterparts in the structure that's passed in
  It uses the same list and generates the same variables
   *)
let rec assign_stitch_variables (seed: string) (structname: string) el = match el with
  [] -> seed ^ "\n"
  | head::tail -> let (typ, name, exp) = head in
    assign_stitch_variables (seed ^ structname ^ "." ^ name ^ " = " ^ name ^ ";\n") (structname) tail

(* This generates the loop after each stitch loops that will resolve the accumulator variables. 
Right now this only works with int accumulators, as accumulators are unfinished at the time
of this submission
 *)
let rec resolve_accums (seed: string) (structname: string ) el = match el with
  [] -> seed ^ "\n"
  | head::tail -> let (typ, name, exp) = head in
    (match typ with
      (Tintap | Tfloatap) -> resolve_accums (seed ^ name ^ "+=" ^ structname ^ "." ^ name ^";\n") structname tail 
      |_ -> resolve_accums seed structname tail)


let rec string_of_c_matrixlist (seed: string) el = match el with
    [] -> seed ^ "}"
    | head::tail -> string_of_c_matrixlist (seed ^ string_of_arraylist head ^ ",\n") tail

let string_of_c_matrixdecl m = string_of_c_dataType m.matrixdecl_type ^ " " ^ m.matrixdecl_name ^ "[" ^
    string_of_expr m.matrixdecl_rows ^ "][" ^ string_of_expr m.matrixdecl_cols ^ "]"

(* Converts a stitch loop into a for loop that creates all the threading information.
  Allocates the threadpool and the structpool, using the procedurally generated function suffix
   *)
let convert_stitch_2_for var start s_end stride fname scope =
  let size = string_of_c_expr s_end in
  let threads = "\npthread_t *threadpool" ^ fname ^ " = malloc(NUMTHREADS * sizeof(pthread_t));\n" in 

  (* Assign the initial variables into the struct *)
  let thread_assignment = "info"^fname^"[thread"^fname^"].begin = i;\n" ^
                                  (assign_stitch_variables "" ("info"^fname^"[thread"^fname^"]") scope.vars )^
                                  "if((" ^ string_of_c_expr var ^ " + 2*(" ^ size ^ "/NUMTHREADS)) > " ^ size ^ ") {\n" ^
                                  "info"^fname^"[thread"^fname^"].end = " ^ size ^ ";\n" ^
                                  string_of_c_expr var ^ " = " ^ size ^ ";\n" ^
                                  "}\n" ^
                                  "else {\n" ^
                                  "info"^fname^"[thread"^fname^"].end = " ^ string_of_c_expr var ^ " + " ^ size ^ "/NUMTHREADS;\n" ^
                                  "}\n" in 

  (* Code to generate the threadpool *)
  let threadgen = "int e = pthread_create(&threadpool"^fname^"[thread"^fname^"], NULL, " ^ fname ^ ", &info"^fname^"[thread"^fname^"]);\n" ^
                  "if (e != 0) {\n" ^
                  "perror(\"Cannot create thread!\");\n" ^
                  "free(threadpool"^fname^"); //error, free the threadpool\n" ^
                  "exit(1);\n" ^
                  "}\n" in

  (* Code that blocks and waits for the threads to finish *)
  let threadjoin = "//loop and wait for all the threads to finish\n" ^
                    "for(" ^ string_of_c_expr var ^ " = 0; "^ string_of_c_expr var ^
                    " < NUMTHREADS; "^string_of_c_expr var ^"++) {\n" ^
                    "pthread_join(threadpool"^fname^"["^ string_of_c_expr var ^"], NULL);\n" ^
                    "}\n" in
  
  (* The loop at the end to resolve any accumulators, if they were used *)
  let accums = "//now we loop and resolve any accumulators\n" ^
                    "for(" ^ string_of_c_expr var ^ " = 0; "^ string_of_c_expr var ^
                    " < NUMTHREADS; "^string_of_c_expr var ^"++) {\n" ^
                    (resolve_accums "" ("info"^fname^"["^string_of_c_expr var^"]") scope.vars) ^
                    "}\n\n" in               

  let varinfo = "struct stch_rangeInfo" ^ fname ^ " *info"^fname^" = malloc(sizeof(struct stch_rangeInfo" ^fname^") * NUMTHREADS);\n" in
  let incr = string_of_c_expr s_end ^ "/" ^ "NUMTHREADS" in
  let loop = threads ^ varinfo ^ "int thread"^fname^" = 0;\n" ^ "for(" in 
  loop ^ string_of_c_expr var ^ " = " ^ string_of_c_expr start ^ ";" ^ string_of_c_expr var ^ " < " ^
    string_of_c_expr s_end ^ ";" ^ string_of_c_expr var ^ " = " ^ string_of_c_expr var ^ "+" ^ incr ^ 
    ") {\n" ^ thread_assignment ^ threadgen ^ "thread"^fname^"++;\n" ^ "}\n\n" ^ threadjoin ^ accums

(* String of c statements. The optional variable here is not ever used, but I'm afraid to take it out
  right before we submit in case it breaks anything
   *)
let rec string_of_c_stmt ?structname:(structname="") (st: c_stmt)= match st with
    C_Block(_, stmts) ->
      "{\n" ^ String.concat "" (List.map (string_of_c_stmt ~structname:"hello") stmts) ^ "}\n"
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
  | C_Stitch(var, start, s_end, stride, fname, body, scope) -> convert_stitch_2_for var start s_end stride fname scope
  | C_Assign(v, e) -> string_of_c_vdecl v ^ " = " ^ string_of_c_expr e ^ ";\n"
  | C_ArrayDecl(a) -> string_of_c_arraydecl a ^ ";\n"
  | C_ArrayInit(arraydecl, el) -> string_of_c_arraydecl arraydecl ^ " = {" ^ String.concat ", " (List.map string_of_expr el) ^ "};\n"
  | C_MatrixDecl(m) -> string_of_c_matrixdecl m ^ ";\n"
  | C_MatrixInit(mdecl, li) -> string_of_c_matrixdecl mdecl ^ " = " ^ string_of_c_matrixlist "{" li ^ ";\n"
  | C_Break -> "break;"


(* This function will take in a structname, a symtable, and a list of statements.
   It will check to see if the statements need to be prepended with the structname by
   checking the symtable, and do so if it needs to
   This function is only for stitch loops *)

 let rec string_of_stch_stmt (structname: string) (table: symTable) (st: c_stmt) = match st with
    C_Block(_, stmts) ->
      "{\n" ^ String.concat "" (List.map (string_of_stch_stmt structname table) stmts) ^ "}\n"
  | C_Expr(_, e) -> string_of_stch_expr structname table e ^ ";\n";
  | C_Vdecl(v) -> (* string_of_c_dataType v.vdecl_type ^ " " ^ v.vdecl_name ^ ";\n"; *)
      if List.exists( fun(_,s,_) -> s = v.vdecl_name) table.vars then
          string_of_c_dataType v.vdecl_type ^ " " ^ structname ^ "->" ^ v.vdecl_name ^ ";\n"
      else
          string_of_c_dataType v.vdecl_type ^ " " ^ v.vdecl_name ^ ";\n"  
  | C_Return(_, c_expr) -> "return " ^ string_of_c_expr c_expr ^ ";\n";
  | C_If(e, s, C_Block(_, [])) -> "if (" ^ string_of_stch_expr structname table e ^ 
      ")\n" ^ string_of_stch_stmt structname table s
  | C_If(e, s1, s2) ->  "if (" ^ string_of_stch_expr structname table e ^ ")\n" ^
      string_of_stch_stmt structname table s1 ^ "else\n" ^ string_of_stch_stmt structname table s2
  | C_For(e1, e2, e3, s) ->
      "for (" ^ string_of_stch_expr structname table e1  ^ " ; " ^ string_of_stch_expr structname table e2 ^
        " ; " ^ string_of_stch_expr structname table e3  ^ ") " ^ string_of_stch_stmt structname table s
  | C_While(e, s) -> "while (" ^ string_of_stch_expr structname table e ^ ") " ^ string_of_stch_stmt structname table s
  | C_Stitch(var, start, s_end, stride, fname, body, scope) -> convert_stitch_2_for var start s_end stride fname scope
 
  (* Assign doesn't need to be checked, it is a variable declaration *)
  | C_Assign(v, e) ->
       string_of_c_vdecl v ^ " = " ^ string_of_stch_expr structname table e ^ ";\n" 

  (* Array declarations don't need to be checked for struct addition *)
  | C_ArrayDecl(a) ->
      string_of_c_arraydecl a ^ ";\n"  

  (* Array inits do not need to be checked for symtable locations *)
  | C_ArrayInit(a, el) ->
      string_of_c_arraydecl a ^ " = {" ^ String.concat ", " (List.map string_of_expr el) ^ "};\n" 

  (* Matrix declarations don't need to be checked *)
  | C_MatrixDecl(m) -> (* string_of_c_matrixdecl m ^ ";\n" *)
       string_of_c_matrixdecl m ^ ";\n"

  | C_MatrixInit(mdecl, li) -> string_of_c_matrixdecl mdecl ^ " = " ^ string_of_c_matrixlist "{" li ^ ";\n" 
  | C_Break -> "break;" 


(* Stitch to func will turn the contents of the stitch loop into a function that is passed through
  to each thread. This will properly generate the for loop that runs at the top of the function, 
  with each thread starting and ending at locations determined by the initial division of labor
   *)
let rec stitch2func = function
    C_Block(_, stmts) ->
      String.concat "" (List.map stitch2func stmts)
  | C_If(e, s, C_Block(_, [])) -> stitch2func s
  | C_If(e, s1, s2) -> stitch2func s2
  | C_For(e1, e2, e3, s) -> stitch2func s
  | C_While(e, s) -> stitch2func s
  | C_Stitch(var, start, s_end, stride, fname, body, scope) ->
     let inner = String.concat "\n" (List.map ((string_of_stch_stmt ("((struct stch_rangeInfo" ^ fname ^ " *)vars)")) scope) body) in 
      "struct stch_rangeInfo" ^ fname ^ " {\n" ^ "int begin;\n"^ "int end;\n" ^ "int stepSize;\n" ^ 
      (print_stitch_variables "" scope.vars) ^ "\n};\n\n" ^ "void *" ^ fname ^ " (void *vars) {\n " ^
      "int "^(string_of_c_expr var)^" = 0;\n for("^(string_of_c_expr var)^" = ((struct stch_rangeInfo"^
      fname^" *)vars)->begin; "^(string_of_c_expr var)^" < ((struct stch_rangeInfo"^fname^
        " *)vars)->end; "^(string_of_c_expr var)^"++) {\n" ^ inner ^ "\n}\nreturn (void*)0;\n}\n"
  | _ -> ""
  
let string_of_stitch func = String.concat "" (List.map stitch2func func.body) 

let string_of_c_fdecl fdecl = match fdecl.fdecl_name with
  "main" -> ""
  | _ -> string_of_c_dataType fdecl.fdecl_type ^ " " ^ fdecl.fdecl_name ^ "(" ^ 
    String.concat ", " (List.map string_of_c_vdecl fdecl.fdecl_formals) ^ ")\n{\n" ^
    String.concat "" (List.map string_of_c_stmt fdecl.body) ^ "}\n"


let string_of_main fdecl = match fdecl.fdecl_name with
  "main" -> string_of_c_dataType fdecl.fdecl_type ^ " " ^ fdecl.fdecl_name ^ "(" ^ 
    String.concat ", " (List.map string_of_c_vdecl fdecl.fdecl_formals) ^ ")\n{\n" ^
    String.concat "" (List.map string_of_c_stmt fdecl.body) ^ "}\n"
  | _ -> ""

let string_of_vars (_, s, _) = s

let string_of_c_program (prog : Stch_cast.c_program ) =
  String.concat "" (List.map string_of_c_stmt prog.stmts) ^ "\n" ^
  String.concat "\n" (List.map string_of_c_fdecl prog.funcs) ^ "\n" ^
  String.concat "\n" (List.map string_of_stitch prog.funcs) ^ "\n" ^ 
  String.concat "\n" (List.map string_of_main prog.funcs) ^ "\n" 



