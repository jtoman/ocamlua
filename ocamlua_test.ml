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
open OUnit;;

let (>>::) n f  = 
  n >:: (OUnit.bracket (fun () -> Ocamlua.init_state ()) f 
	(fun _ -> ()));;

let identity = "
function id(x)
   return x
end";;

let test_conv state = 
  Ocamlua.eval_string state identity;
  List.iter
	(fun x ->
	  assert_equal x
		(Ocamlua.call state "id" [x])
	) [`Lua_Number 4.0; `Lua_String "4"; `Lua_String "foo"; `Lua_Nil; `Lua_Boolean true];;

let test_simple_table state = 
  let table = `Lua_Table [
	(`Lua_Number 1.0, `Lua_Number 3.0);
	(`Lua_Number 2.0, `Lua_Number 4.0);
	(`Lua_Number 3.0, `Lua_String "foo")
  ] in
  Ocamlua.eval_string state "
function foo(t)
  local c = table.remove(t,1)
  table.insert(t,c)
  return t
end
";
  let ret = Ocamlua.call state "foo" [table] in
  match ret with
  | `Lua_Table x -> 
	assert_equal 3 (List.length x);
	assert_equal (`Lua_Number 4.0) (List.assoc (`Lua_Number 1.0) x);
	assert_equal (`Lua_String "foo") (List.assoc (`Lua_Number 2.0) x);
	assert_equal (`Lua_Number 3.0) (List.assoc (`Lua_Number 3.0) x);
  | _ -> assert_failure "returned value was not a table";;

let test_callback state = 
  Ocamlua.eval_string state ("
function t(a)
   return a(4)
end
");
  let ret = Ocamlua.call state "t" [`Lua_Closure (
	function 
	| `Lua_Number x -> assert_equal 4.0 x;
	  `Lua_Number (x +. 6.0)
	| _ -> assert_failure "got a different result type"
  )] in
  assert_equal (`Lua_Number 10.0) ret;;

let test_garbage_collection () = 
  (* tests that the lua state is released from the weak global list of states
     once all real references have been dropped *)
  let flag = ref false in
  let record = ref None in
  let finalizer _ = flag := true in
  let f g = 
    let state = Ocamlua.init_state () in
    Gc.finalise finalizer state;
    Ocamlua.eval_string state identity;
    g state in
  let a s = 
    let closure _ = Ocamlua.eval_string s "
function foo(x) return x + 1 end
"; `Lua_Nil in
    match Ocamlua.call s "id" [`Lua_Closure closure] with
      | `Lua_Closure x -> x
      | _ -> assert_failure "unexpected type"
  in
  let test_body () = 
    record := Some (f a);
    (* at this point, the only reference to the state is from the environment
       of the returned closure (the s variable above).
       We still have a reference to the state from the weak hash table, but
       after the closure in record is freed then the state should become
       unreachable and be collected
    *)
    Gc.full_major ();
    (* the above ensures that everything that is not reachable is collected.
       if the state was collected then the finalizer should have run
       and thus the flag will be true (which means a failure *)
    assert_bool "The state was collected early!" (not !flag);
    record := None;
    ()
  in
  test_body(); Gc.full_major(); assert_bool "The state was not collected" (!flag);;

let test_complex_callback state = 
  let func x = 
	Ocamlua.call state "t" [x]
  in
  Ocamlua.eval_string state "
function t(x)
  return x + 1
end
function a(x,y)
  return y(x.foo)
end
";
  let ret = Ocamlua.call state "a" [
	`Lua_Table [
	  (`Lua_String "foo", `Lua_Number 5.0)
	];
	`Lua_Closure func
  ] in
  assert_equal (`Lua_Number 6.0) ret;;

let test_higher_order_functions state = 
  let f = function
    | `Lua_Closure g -> g (`Lua_Number 4.0)
    | _ -> assert_failure "The passed value was not the expected type"
  in
  let g = function
    | `Lua_Number x -> `Lua_String (Printf.sprintf "%d" (int_of_float x))
    | _ -> assert_failure "the passed value was not the expected type"
  in
  Ocamlua.eval_string state "
function hof(f,g)
  return f(g)
end
";
  match Ocamlua.call state "hof" [`Lua_Closure f; `Lua_Closure g] with
    | `Lua_String s -> assert_equal s "4"
    | _ -> assert_failure "Returned value was not the expected type";;

let test_syntax_error state =
  assert_raises
    (Ocamlua.Syntax_error "[string \"funtion foo(x) return x + 1 end\"]:1: syntax error near 'foo'")
    (fun () -> 
      Ocamlua.eval_string state "funtion foo(x) return x + 1 end")

let test_runtime_error state = 
  assert_raises
    (Ocamlua.Runtime_error "[string \"function a() return 5 + \"a\" end\"]:1: attempt to perform arithmetic on a string value")
    (fun () ->
      Ocamlua.eval_string state "function a() return 5 + \"a\" end";
      Ocamlua.call state "a" []
    )

let test_conversion_errors state = 
  Ocamlua.eval_string state "
  co = coroutine.create(function() return nil end)
  function a(f)
    f(co)
  end
  function b()
    return co
  end
";
  (* bad argument conversions when calling into ocaml become lua exceptions *)
  assert_raises
    (Ocamlua.Runtime_error "[string \"...\"]:4: Bad arguments to ocaml callback")
    (fun () -> Ocamlua.call state "a" [`Lua_Closure (fun x -> x)]);
  (* conversions when returning to ocaml code become bad value exceptions *)
  assert_raises
    Ocamlua.Bad_value
    (fun () -> Ocamlua.call state "b" []);;

let test_closure_garbage_collection () = 
  let collected_flag = ref false in
  let finalizer _ = collected_flag := true in
  let f y = 
    let state = Ocamlua.init_state () in
    Ocamlua.eval_string  (state) "
f = nil
function a(my_f)
  f = my_f
end
";
    (* these three lines ensure that closure (heap allocated thanks to
       the capure of the parameter y) is installed as the global f in
       the lua environment. After this function returns, there will be
       no more references to this closure from within this function *)
    let closure = function
      | `Lua_Number x -> `Lua_Number (x +. y)
      | _ -> assert_failure "the argument passed in was not of the expected type"
    in
    Gc.finalise finalizer closure;
    let _ = Ocamlua.call (state) "a" [`Lua_Closure closure] in
    state 
  in
  let g () = 
    let state = f 5.0 in
    (* now the only reference to the method in the closure is via the
       hash map associated with the lua state *)
    Gc.full_major ();
    (* ensure that it didn't get picked up by garbage collection *)
    assert_bool "Closure was collected early!" (not !collected_flag);
    (* make sure that we can stil call it *)
    assert_equal  (Ocamlua.call (state) "f" [`Lua_Number 4.0]) (`Lua_Number 9.0) in
  g ();
  (* after returning the state created in f (called by g) has fallen
     out of scope and is garbage. Run garbage collection and then make
     sure that as the state went out of scope, the closure got
     collected too *)
  Gc.full_major ();
  assert_bool "Closure was not collected!" (!collected_flag);;

let test_gc_metamethod state = 
  Ocamlua.eval_string state "
m = {
  __gc = function(obj) return 4 + \"a\" end
}
function do_test()
  a = {}
  setmetatable(a, m)
  a = {}
  collectgarbage(\"collect\")
end
";
  assert_raises
    (Ocamlua.Internal_error (Ocamlua.GC_metamethod "error in __gc metamethod ([string \"...\"]:3: attempt to perform arithmetic on a string value)"))
    (fun () ->
      ignore (Ocamlua.call state "do_test" [])
    );;

let test_recursion_detection state = 
  Ocamlua.eval_string state "
function bad_function()
  a = {}
  a[\"foo\"] = a
  return a
end
function bad_function_2()
  a = {}
  a[\"next\"] = {}
  a[\"next\"][\"next\"] = {}
  a[\"next\"][\"next\"][\"next\"] = a
  return a
end
b = {}
function bad_global()
  b[\"foo\"] = b
  return b
end
function good_global()
  b[\"foo\"] = 1
  return b
end
";
  assert_raises
    Ocamlua.Bad_value (fun () -> Ocamlua.call state "bad_function" []);
  assert_raises
    Ocamlua.Bad_value (fun () -> Ocamlua.call state "bad_function_2" []);
  (* this tests that the recursion set gets reset every time *)
  assert_raises
    Ocamlua.Bad_value (fun () -> Ocamlua.call state "bad_global" []);
  try
    assert_equal (`Lua_Table [(`Lua_String "foo", `Lua_Number 1.0)]) (Ocamlua.call state "good_global" [])
  with _ -> assert_failure "Exception raised where none was expected";;

let test_error_propagation state = 
  let test_success_flag = ref false in
  Ocamlua.eval_string state "
function foo(f)
  f(nil)
end
function bar()
  error(\"some error\")
end
";
  let closure _ = 
    try 
      Ocamlua.call state "bar" []
    with Ocamlua.Runtime_error s as e-> 
      test_success_flag := s = "[string \"...\"]:6: some error";
      raise e
  in
  assert_raises
    (Ocamlua.Runtime_error "[string \"...\"]:3: Exception in ocaml callback")
    (fun () ->
      Ocamlua.call state "foo" [`Lua_Closure closure]
    );
  assert_bool "Caught exception was not the expected exception" (!test_success_flag);;

let test_list_conversions () = 
  (* basic test *)
  let basic_table = [
      (`Lua_Number 1.0, `Lua_String "hello");
      (`Lua_Number 2.0, `Lua_String "world");
      (`Lua_Number 3.0, `Lua_String "!")
    ] in
  assert_equal [`Lua_String "hello"; `Lua_String "world"; `Lua_String "!"] (Ocamlua.list_of_table basic_table);
  (* make sure out of order tables are handled nicely *)
  let out_of_order_table =
    [
      (`Lua_Number 2.0, `Lua_String "hello");
      (`Lua_Number 1.0, `Lua_Nil);
      (`Lua_Number 4.0, `Lua_Number 3.0);
      (`Lua_Number 3.0, `Lua_String "world")
    ] in
  assert_equal [`Lua_Nil; `Lua_String "hello"; `Lua_String "world"; `Lua_Number 3.0] (Ocamlua.list_of_table out_of_order_table);
  (* do we work for empty tables? *)
  assert_equal [] (Ocamlua.list_of_table []);
  (* test bad types for keys *)
  List.iter (fun l ->
    assert_raises (Failure "non-numeric key") (fun () ->
      Ocamlua.list_of_table l))
    [
      [(`Lua_String "foo", `Lua_String "bar")];
      [(`Lua_Number 1.0, `Lua_String "foo");
       (`Lua_Number 2.0, `Lua_String "bar");
       (`Lua_Boolean true, `Lua_String "oops")
      ]
    ];
  (* test bad index values *)
  List.iter (fun l ->
    assert_raises (Failure "unexpected numeric index") (fun () ->
      Ocamlua.list_of_table l)) 
    [
      [(`Lua_Number 1.0, `Lua_String "bar");
       (`Lua_Number 1.2, `Lua_String "oh no")];
      [(`Lua_Number 0.0, `Lua_String "oh no");
       (`Lua_Number 1.0, `Lua_String "bar")];
      [(`Lua_Number 0.5, `Lua_String "w")]
    ];
  (* finally, test conversions TO tables (this is relatively easy *)
  assert_equal (`Lua_Table []) (Ocamlua.table_of_list []);
  let rec test_loop table expected_ind l = match l with
    | [] -> ()
    | h::t -> assert_equal h (List.assoc (`Lua_Number expected_ind) table);
        test_loop table (expected_ind +. 1.0) t
  in
  let input_list = [`Lua_Nil; `Lua_Boolean false; `Lua_String "qux"; `Lua_Number 3.0] in
  match (Ocamlua.table_of_list input_list) with
    | `Lua_Table table -> test_loop table 1.0 input_list
    | _ -> assert_failure "Returned type was unexpected";;

let suite = 
  "basic">:::
	["test_conv">>:: test_conv;
	 "test_list_conversions" >:: test_list_conversions;
	 "test_callback">>:: test_callback;
	 "test_table">>:: test_simple_table;
	 "test_complex_callback">>::test_complex_callback;
     "test_garbage_collection" >:: test_garbage_collection;
     "test_closure_garbage_collection" >:: test_closure_garbage_collection;
     "test_higher_order_functions" >>:: test_higher_order_functions;
     "test_syntax_error" >>:: test_syntax_error;
     "test_runtime_error" >>:: test_runtime_error;
     "test_conversion_errors" >>:: test_conversion_errors;
     "test_gc_metamethod" >>:: test_gc_metamethod;
     "test_error_propagation" >>:: test_error_propagation;
     "test_recursion_detection" >>:: test_recursion_detection
	];;

let _ = 
  run_test_tt_main suite;;
