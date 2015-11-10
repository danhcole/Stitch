(* Stitch Compiler *)

open Stch_ast

let rec expr = function 
Literal i -> print(i)
;;