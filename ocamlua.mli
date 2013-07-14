(*
   Copyright 2013 John Toman

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*)
type lua_state
type lua_value = [ `Lua_Table of (lua_value * lua_value) list
					  | `Lua_Nil
					  | `Lua_String of string
					  | `Lua_Number of float
					  | `Lua_Boolean of bool
					  | `Lua_Closure of lua_closure ]
and lua_closure = lua_value -> lua_value
val table_of_list : lua_value list -> lua_value
val list_of_table : lua_value -> lua_value list
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
