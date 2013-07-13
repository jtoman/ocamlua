type lua_state
type lua_value = [ `Lua_Table of (lua_value * lua_value) list
					  | `Lua_Nil
					  | `Lua_String of string
					  | `Lua_Number of float
					  | `Lua_Boolean of bool
					  | `Lua_Closure of lua_closure] 
and lua_closure = lua_value -> lua_value
type func_map = (int, lua_closure) Hashtbl.t
external create_state : func_map -> lua_state = "ocamlua_create_state";;
let init_state () = 
  let f_map = Hashtbl.create 19 in
  create_state f_map;;

let table_of_list l = 
  let rec loop (pair_accum,index) = function
    | [] -> `Lua_Table pair_accum
    | h::t -> loop ((`Lua_Number index,h)::pair_accum, index +. 1.0) t 
  in
  loop ([],1.0) l;;

let list_of_table = function
  | `Lua_Table l -> 
      let cmp (a,_) (b,_) = match (a,b) with
        | (`Lua_Number a',`Lua_Number b') -> compare a' b'
        | _ -> failwith "non-numeric key" in
      let sorted_list = List.sort cmp l in
      let rec loop index l = match l with
        | [] -> []
        | (`Lua_Number i,v)::t when i = index -> 
            v::(loop (index +. 1.0) t)
        | (`Lua_Number _,v)::_ -> failwith "unexpected numeric index"
        | _ -> failwith "non-numeric key" (* non-numeric keys can slip through if the user passes in a table with one element *)
      in
      loop 1.0 sorted_list
  | _ -> failwith "passed value was not a table"
      
external load_file : lua_state -> string -> unit = "ocamlua_load_file";;
external call : lua_state -> string -> lua_value list -> lua_value = "ocamlua_call";;
external eval_string : lua_state -> string -> unit = "ocamlua_eval_string";;

let meta_map = ref (Weak.create 50);;

let register_state : lua_state * func_map -> int = fun s ->
  let len = Weak.length (!meta_map) in
  let rec loop i = 
    if i == len then
      let new_meta_map = Weak.create (len * 3 / 2 + 2) in
      Weak.blit (!meta_map) 0 new_meta_map 0 len;
      meta_map := new_meta_map;
      Weak.set (!meta_map) i (Some s);
      i
    else
      match Weak.get (!meta_map) i with
        | Some _ -> loop (succ i)
        | None -> Weak.set (!meta_map) i (Some s); i
  in
  loop 0;;

let recover_state : int -> lua_state * func_map = fun s_id ->
  match Weak.get (!meta_map) s_id with
    | None -> raise Not_found
    | Some x -> x;;

let register_closure = fun s_id c_id f ->
  let (_,tbl) = recover_state s_id in
  Hashtbl.add tbl c_id f;;

let fetch_closure = fun s_id c_id ->
  let (_,tbl) = recover_state s_id in
  Hashtbl.find tbl c_id;;

let call_closure = fun s_id c_id v ->
  let f = fetch_closure s_id c_id in
  f v;;

Callback.register "ocamlua.register_state" register_state;;
Callback.register "ocamlua.register_closure" register_closure;;
Callback.register "ocamlua.fetch_closure" fetch_closure;;
Callback.register "ocamlua.call_closure" call_closure;;
Callback.register "ocamlua.recover_state" recover_state;;

type internal_error_code = GC_metamethod of string
							| Err_message_handler of string
							| Out_of_memory

exception No_such_method of string;;
let _ = Callback.register_exception "ocamlua-no-such-method" (No_such_method "");;
exception Syntax_error of string;;
let _ = Callback.register_exception "ocamlua-syntax-error" (Syntax_error "");;
exception Runtime_error of string;;
let _ = Callback.register_exception "ocamlua-runtime-error" (Runtime_error "");;
exception Internal_error of internal_error_code;;
let _ = Callback.register_exception "ocamlua-internal-error" (Internal_error Out_of_memory);;
exception Bad_value;;
let _ = Callback.register_exception "ocamlua-bad-value" (Bad_value);;

