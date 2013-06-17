type lua_state
type lua_value = [ `Lua_Table of (lua_value * lua_value) list
					  | `Lua_Nil
					  | `Lua_String of string
					  | `Lua_Number of float
					  | `Lua_Boolean of bool
					  | `Lua_Closure of lua_closure ]
and lua_closure = lua_value -> lua_value
(*val list_to_table : lua_value list -> lua_value
val table_to_list : lua_value -> lua_value list*)
val init_state : unit -> lua_state;;
external load_file : lua_state -> string -> unit = "ocamlua_load_file";;
external call : lua_state -> string -> lua_value list -> lua_value = "ocamlua_call";;
external eval_string : lua_state -> string -> unit = "ocamlua_eval_string";;

type internal_error_code = GC_metamethod of string
							| Err_message_handler of string
							| Out_of_memory

exception No_such_method of string;;
exception Syntax_error of string;;
exception Runtime_error of string;;
exception Internal_error of internal_error_code;;
exception Bad_value;;
