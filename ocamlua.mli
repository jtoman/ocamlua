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
(** A bridge between the OCaml and Lua runtimes that allows OCaml code
    to call into Lua code. *)

type lua_state
(** An opaque datatype representing the underlying Lua state *)

type lua_value = [ `Lua_Table of lua_table
				 | `Lua_Nil (** The empty value *)
				 | `Lua_String of string
				 | `Lua_Number of float (** The numeric type in Lua. Lua does not have a separate type for integers: all numeric values are of type float (this actually depends on the compile flags used for the Lua runtime, but this library will only compile if floats are used for Lua numbers *)
				 | `Lua_Boolean of bool
				 | `Lua_Closure of lua_closure ]
(** Values that can be shared between the Lua runtime and OCaml code *)
and lua_table =  (lua_value * lua_value) list
(** An associative list corresponding to a Lua table. As in Lua, keys
    may be any valid value. In the case of duplicate keys, the last
    key in the list "wins". *)
and lua_closure = lua_value -> lua_value;;
(** The type of a callback from Lua code to Ocaml code *)

val init_state : unit -> lua_state;;
(** Creates a new {!lua_state}. Each state created by this function is
    completely indepedent from one another: any side effects or operations
    in one state are not visible to any other state created by this
    function.
    
    Some important implementation notes that may affect how you use
    the returned state: when you pass an Ocaml closure into Lua code
    as an argument to {!call} a reference to the passed closure is
    saved in a hash associated with the Lua state. This hash is not
    released until the Lua state itself is released, so be aware that
    if you pass in several distinct closures you may experience a
    stress on memory.
*)

external load_file : lua_state -> string -> unit = "ocamlua_load_file";;
(** [call state fname] loads the Lua source code contained in the file
    [fname] and immediately evaluates it. If the loaded code returns a
    value it is silently discarded.
    @raise No_such_file if [fname] can not be loaded by the Lua runtime
    @raise Syntax_error if the code contained in [fname] is not syntatically valid Lua code
    @raise Internal_error if during the execution of the code loaded from [fname] an internal error occurs
    @raise Runtime_error if during the execution of the code loaded from [fname] a runtime error occurs
*)
external call : lua_state -> string -> lua_value list -> lua_value = "ocamlua_call";;
(** [call state f args] calls the function accessible via the global
    name [f] in the state [state] with the (possibly zero) arguments
    [args]. The return value of this function is the value returned by
    [f]. If [f] returns more than 1 value they are silently discarded, if
    it returns 0 values then [`Lua_Nil] is returned.
    @raise Runtime_error if a Lua error is generated during the execution of [f] or an OCaml callbaack raises an exception (see {!Runtime_error} for details)
    @raise No_such_method if [f] does not exist in the global environment associated with [state]
    @raise Internal_error if a Lua internal error occurs
    @raise Bad_value if [f] returns a value that cannot be represented by the {!lua_value} type. *)
external eval_string : lua_state -> string -> unit = "ocamlua_eval_string";;
(** [eval_string state code] immediately loads and executes the Lua
    code in the string [code] in the context of [state]. Any side
    effects or actions taken during the execution of [code] will not
    be visible to any other states. It is not possible to change the
    environment of the loaded chunk. If the loaded chunk returns a value it is
    silently discarded.
    @raise Syntax_error if the Lua code in [code] is not syntatically valid
    @raise Runtime_error if a runtime error occurs during the execution of the loaded chunk
    @raise Internal_error if a Lua internal error occurs during the evaluation of the string
*)

(** {3 Helper functions} *)

val table_of_list : lua_value list -> lua_value
(** Coverts a list Lua values into a Lua vector (with indices starting from 1). *)
val list_of_table : lua_table -> lua_value list
(** Converts a table of size n with keys 1, 2, ... n+1 into a list of Lua values. If the keys are of a non-numeric type or the keys do not have the expected order will throw a [Failure] exception. *)

(** {2 Exceptions} *)

type internal_error_code = GC_metamethod of string (** Thrown when an error occurs during the execution of a __gc metamethod on an object. The string argument is the error message reported by the Lua runtime *)
						   | Err_message_handler of string (** Thrown when an error occurs during the execution of a user installed error handler. The string argument is the error message reported by the Lua runtime *)
						   | Out_of_memory (** Thrown when the Lua runtime runs out of memory. In such a situation the Lua runtime does not report any error messages *)
(** Types of exceptional conditions that may occur during the
    execution of the Lua runtime *)

exception No_such_method of string;;
(** Raised by {!call} if the method to call does not exist. The string
    argument is the name of the function that does*)

exception Syntax_error of string;;
(** Raised by {!eval_string} and {!load_file} if the Lua source
    code being loaded has a syntax error. The string is the error
    reported by the Lua runtime *)

exception Runtime_error of string;;
(** Raised if a runtime error occurs during the execution of Lua
    code. This includes runtime errors generated by the Lua runtime
    (arithmetic errors, no such method, etc.) and errors generated via
    the error function. Exceptions thrown inside of an Ocaml callback
    are converted into a Lua error with the error string "Exception in
    ocaml callback" and if not caught, may propagate to the top level
    and become a Runtime_error (in this case the original exception
    information is lost). *)

exception Internal_error of internal_error_code;;
(** Thrown when more "exotic" errors are thrown by the Lua runtime. It
    is expected that these errors will be more uncommon and as such
    the various error conditions are represented by tag of the
    {!internal_error_code} argument. *)

exception Bad_value;;
(** Thrown when an error occurs converting a Lua value into an Ocaml
    representation to return to the calling code (note that the
    opposite can never happen, all values allowed by the {!lua_value}
    type can be represented as Lua values, up to the practical limits
    of the system). There are two ways a converion may fail: when the
    conversion of a value of an inconvertible type is attempted
    (e.g. a coroutine thread, arbitrary userdata, etc.)  or when in
    the process of converting a table recurion is detected.
    
    This exception is only thrown when a coversion error occurs when
    converting a value to return back into OCaml code. When conversion
    fails when attempting to call back into Ocaml code, the conversion
    error is converted into a Lua error, with the string "Bad
    arguments to ocaml callback". *)

exception No_such_file of string;;
(** Thrown by {!load_file} when the Lua cannot load code from the
    given file. *)
