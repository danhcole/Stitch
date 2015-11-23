open Stch_ast;;

module VNFMap = Map.Make(String);; (* Variables and Functions *)

type VNFInfo = {
	id 		: string;
	typ 	: datatype;
	arg_typ	: datatype list;
	arg_id	: string list;
}

type env = {
	VNF_stack: datatype VNFMap.t list;
	current_function: string;
}

let create =
{
	VNF_stack = VNFMap.empty::[];
	current_function = "";
}

let update v_stack curr_f = 
{
	VNF_stack = v_stack;
	current_function = curr_f;
}

