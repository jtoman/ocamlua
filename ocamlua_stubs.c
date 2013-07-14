/*
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
*/
#include<caml/mlvalues.h>
#include<caml/memory.h>
#include<caml/custom.h>
#include<caml/alloc.h>
#include<caml/callback.h>
#include<caml/fail.h>
#include<assert.h>
#include<lua5.2/lua.h>
#include<lua5.2/lauxlib.h>
#include<lua5.2/lualib.h>

#define FUNCTION_BRIDGE_METATABLE "ocamlua.method_proxy"
#define RELEASE_CLOSURES "ocamlua.release_closures"
#define OcamLuaState_val(v) (((ocamlua_state_t*)Data_custom_val(v)))
#define OcamLuaState_tuple(v) (OcamLuaState_val(Field((v),0)))
#define Is_state_live(v) (OcamLuaState_val((v))->L != NULL)

typedef struct ocamlua_state {
  lua_State *L;
  int id;
} ocamlua_state_t;

typedef struct ocamlua_cb {
  int s_id;
  int c_id;
} ocamlua_cb_t;

static void lua_to_table(lua_State*, value, int*);
static value lua_to_value(lua_State *, int *);
static value lua_to_value_rec_check(lua_State *, int *);
static int *get_c_counter(lua_State*);
static int get_sid(lua_State*);
static void ocamlua_destroy_state(value);
static void value_to_lua(lua_State*, value, int, int *);
static value recover_closure(lua_State *, int);
static int ocamlua_func_bridge(lua_State *);
static int _ocaml_func_bridge(lua_State *);
static void code_to_exception(int, lua_State *);


static void ocamlua_destroy_state(value v) {
  lua_close(OcamLuaState_val(v)->L);
  OcamLuaState_val(v)->L = NULL;
}

static struct custom_operations lua_state_ops = {
 "ocamlua.state",
  ocamlua_destroy_state,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};


static int *get_c_counter(lua_State *L) {
  lua_pushstring(L, "ocamlua.c_counter");
  lua_rawget(L, LUA_REGISTRYINDEX);
  int *to_ret = lua_touserdata(L, -1);
  lua_pop(L, 1);
  return to_ret;
}

static int get_sid(lua_State *L) {
  lua_pushstring(L, "ocamlua.id");
  lua_rawget(L, LUA_REGISTRYINDEX);
  int to_ret = lua_tointeger(L, -1);
  lua_pop(L, 1);
  return to_ret;
}


static void value_to_lua(lua_State *L, value v, int s_id, int *c_counter) {
  CAMLparam1(v);
  CAMLlocal1(it);
  if(Is_long(v) && v == hash_variant("Lua_Nil")) {
	lua_pushnil(L);
  } else if(Field(v, 0) == hash_variant("Lua_String")) {
	lua_pushstring(L, String_val(Field(v, 1)));
  } else if(Field(v, 0) == hash_variant("Lua_Number")) {
	lua_pushnumber(L, Double_val(Field(v, 1)));
  } else if(Field(v, 0) == hash_variant("Lua_Boolean")) {
	lua_pushboolean(L, Bool_val(Field(v, 1)));
  } else if(Field(v, 0) == hash_variant("Lua_Closure")) {
	static value *register_closure_f = NULL;
	if(register_closure_f == NULL) {
	  register_closure_f = caml_named_value("ocamlua.register_closure");
	}
	int c_id = (*c_counter)++;
	caml_callback3(*register_closure_f, Val_long(s_id), Val_long(c_id), Field(v, 1));
	ocamlua_cb_t *cb = lua_newuserdata(L, sizeof(ocamlua_cb_t));
	cb->c_id = c_id;
	cb->s_id = s_id;
	luaL_setmetatable(L, FUNCTION_BRIDGE_METATABLE);
  } else {
	lua_newtable(L);
	int table_ind = lua_absindex(L, -1);
	it = Field(v, 1);
	while(it != Val_int(0)) {
	  value_to_lua(L, Field(Field(it, 0), 0), s_id, c_counter);
	  value_to_lua(L, Field(Field(it, 0), 1), s_id, c_counter);
	  lua_rawset(L, table_ind);
	  it = Field(it, 1);
	}
  }
  CAMLreturn0;
}

static value recover_closure(lua_State *L, int idx) {
  static value *fetch_closure_f = NULL;
  ocamlua_cb_t *cb = (ocamlua_cb_t*)lua_touserdata(L, idx);
  if(fetch_closure_f == NULL) {
	fetch_closure_f = caml_named_value("ocamlua.fetch_closure");
  }
  return caml_callback2(*fetch_closure_f, Val_long(cb->s_id), Val_long(cb->c_id));
}

static value lua_to_value(lua_State *L, int *error) {
  CAMLparam0();
  CAMLlocal1(ret);
  int type = lua_type(L, -1);
  if(type != LUA_TNIL) {
	ret = caml_alloc(2, 0);
  }
  if(type == LUA_TSTRING) {
	Store_field(ret, 0, hash_variant("Lua_String"));
	Store_field(ret, 1, caml_copy_string(lua_tostring(L, -1)));
  } else if(type == LUA_TNUMBER) {
	Store_field(ret, 0, hash_variant("Lua_Number"));
	Store_field(ret, 1, caml_copy_double(lua_tonumber(L, -1)));
  } else if(type == LUA_TNIL) {
	ret = caml_hash_variant("Lua_Nil");
  } else if(type == LUA_TBOOLEAN) {
	Store_field(ret, 0, hash_variant("Lua_Boolean"));
	Store_field(ret, 1, Val_bool(lua_toboolean(L, -1)));
  } else if(type == LUA_TUSERDATA && lua_getmetatable(L, -1)) {
	luaL_getmetatable(L, FUNCTION_BRIDGE_METATABLE);
	int comp = lua_compare(L, -1, -2, LUA_OPEQ);
	lua_pop(L, 2);
	if(comp != 1) {
	  *error = 1;
	} else {
	  Store_field(ret, 0, hash_variant("Lua_Closure"));
	  Store_field(ret, 1, recover_closure(L, -1));
	}
  } else if(type == LUA_TTABLE) {
    lua_pushstring(L, "ocamlua.recursion_check"); // stack is table | "ocamlua.recursion_check"
    lua_rawget(L, LUA_REGISTRYINDEX); // stack is table | recursion_set
    lua_pushvalue(L, -2); // stack is table | recursion_set | table' where table = table'
    lua_rawget(L, -2); // stack is table | recursion_set | nil or 1
    if(lua_isnil(L, -1)) {
      // this value hasn't been seen before
      lua_pop(L, 1); // stack is table |  recursion_set
      lua_pushvalue(L, -2); // stack is table | recursion_set | table' where table = table'
      lua_pushinteger(L, 1); // stack is table | recursion_set | table' | 1
      lua_rawset(L, -3); // set the recursion_set[table'] = 1
      lua_pop(L, 1); // pop off the recursion set, now we're left with the table as we started out
      Store_field(ret, 0, hash_variant("Lua_Table"));
      lua_to_table(L, ret, error);
    } else {
      // we've seen this table before, error out and return
      lua_pop(L, 2); // pop off the recursion set and the sentinel 1 value, we still have the table which will be popped off below
      *error = 1;
    }
  } else {
	*error = 1;
  }
  lua_pop(L, 1);
  CAMLreturn(ret);
}

static value lua_to_value_rec_check(lua_State *L, int *error) {
  CAMLparam0();
  lua_pushstring(L, "ocamlua.recursion_check");
  lua_createtable(L, 0, 10);
  lua_rawset(L, LUA_REGISTRYINDEX);
  CAMLreturn(lua_to_value(L, error));
}

static void lua_to_table(lua_State *L, value ret, int *error) {
  CAMLparam1(ret);
  CAMLlocal5(k, v, it, tup, cons);
  int t = lua_absindex(L, -1);
  lua_pushnil(L);
  it = Val_int(0);
  while(*error == 0 && lua_next(L, t) != 0) {
	// stack: table | key | value
	lua_pushvalue(L, -2);
	// stack: table | key | value | key
	lua_insert(L, -2);
	// stack: table | key (for iteration) | key (for conv) | value
	v = lua_to_value(L, error);
	// stack: table | key (for it) | key (for conv)
	if(*error == 0) {
	  k = lua_to_value(L, error);
	} else {
	  lua_pop(L, 1);
	}
	// stack: table | key (for it)
	if(*error == 0) {
	  /*
	   * I'm pretty sure I could get by without the tup variable, and use only
	   * the cons value (by doing something like Store_field(Field(cons...)
	   * but there's no way I'm going to be "clever" when it comes to the ocaml
	   * gc
	   */
	  tup = caml_alloc_tuple(2);
	  cons = caml_alloc_tuple(2);
	  Store_field(tup, 0, k);
	  Store_field(tup, 1, v);
	  Store_field(cons, 0, tup);
	  Store_field(cons, 1, it);
	  it = cons;
	} else {
	  // pop off the key, we don't need it anymore
	  lua_pop(L, 1);
	}
  }
  Store_field(ret, 1, it);
  CAMLreturn0;
}

#define SUCCESS 0
#define OCAML_ERROR 1
#define CONVERSION_ERROR 2

static int _ocaml_func_bridge(lua_State *L) {
  CAMLparam0();
  CAMLlocal2(arg, ret);
  static value * call_closure_f = NULL;
  if(call_closure_f == NULL) {
	call_closure_f = caml_named_value("ocamlua.call_closure");
  }
  int s_id = get_sid(L);
  int conv_error = 0;
  int *c_counter = get_c_counter(L);
  ocamlua_cb_t *cb = lua_touserdata(L, 1);
  arg = lua_to_value_rec_check(L, &conv_error);
  if(conv_error) {
	CAMLreturnT(int, CONVERSION_ERROR);
  }
  ret = caml_callback3_exn(*call_closure_f, Val_long(cb->s_id), Val_long(cb->c_id), arg);
  if(Is_exception_result(ret)) {
    CAMLreturnT(int, OCAML_ERROR);
  } else {
    value_to_lua(L, ret, s_id, c_counter);
  }
  CAMLreturnT(int, SUCCESS);  
}

static int ocamlua_func_bridge(lua_State *L) {
  if(lua_gettop(L) != 2) {
	return luaL_error(L, "too many arguments");
  }
  int ret_code = _ocaml_func_bridge(L);
  if(ret_code == OCAML_ERROR) {
    return luaL_error(L, "Exception in ocaml callback");
  } else if(ret_code == CONVERSION_ERROR) {
    return luaL_error(L, "Bad arguments to ocaml callback");
  } else {
    return 1;
  }
}

CAMLprim value ocamlua_create_state(value f_map) {
  CAMLparam1(f_map);
  CAMLlocal2(state_v, t);
  lua_State *L = luaL_newstate();
  luaL_openlibs(L);
  luaL_newmetatable(L, FUNCTION_BRIDGE_METATABLE); // stack top (index 1) has the metatable that will call back into ocaml
  lua_pushstring(L, "__call"); // our magic key
  lua_pushcfunction(L, ocamlua_func_bridge); // the function to associate with the key (the top of the stack is now at index 3)
  lua_rawset(L, 1); // after this line, the key and value will be popped off the stack, all that will be left is the metatable at index 1
  lua_pop(L, 1); // balance the stack, we can get the metatable back by the key
  lua_pushstring(L, "ocamlua.c_counter");
  *((int*)lua_newuserdata(L, sizeof(int))) = 0;
  lua_rawset(L, LUA_REGISTRYINDEX);

  state_v = alloc_custom(&lua_state_ops, sizeof(ocamlua_state_t), 0, 1);
  t = caml_alloc_tuple(2);
  Store_field(t, 0, state_v);
  Store_field(t, 1, f_map);
  int id = Long_val(caml_callback(*caml_named_value("ocamlua.register_state"), t));
  lua_pushstring(L, "ocamlua.id");
  lua_pushinteger(L, id);
  lua_rawset(L, LUA_REGISTRYINDEX);
  OcamLuaState_val(state_v)->L = L;
  OcamLuaState_val(state_v)->id = id;
  CAMLreturn(t);
}

static void code_to_exception(int code, lua_State *L) {
  CAMLparam0();
  CAMLlocal2(arg, msg);
  assert(code != LUA_OK);
  if(code != LUA_ERRMEM) {
	msg = caml_copy_string(lua_tostring(L, -1));
	lua_pop(L, 1);
  }
  switch(code) {
  case LUA_ERRRUN:
	caml_raise_with_arg(*caml_named_value("ocamlua-runtime-error"), msg);
	break;
  case LUA_ERRMEM:
	caml_raise_with_arg(*caml_named_value("ocaml-internal-error"), Val_int(0));
	break;
  case LUA_ERRSYNTAX:
	caml_raise_with_arg(*caml_named_value("ocamlua-syntax-error"), msg);
	break;
  case LUA_ERRGCMM:
  case LUA_ERRERR:
	arg = caml_alloc(1, code == LUA_ERRERR);
	Store_field(arg, 0, msg);
	caml_raise_with_arg(*caml_named_value("ocamlua-internal-error"), arg);
	break;
  default:
    /* impossible case */
	assert(0);
  }
  CAMLreturn0;
}

CAMLprim value ocamlua_load_file(value lua_state, value file) {
  CAMLparam2(lua_state, file);
  ocamlua_state_t *state = OcamLuaState_tuple(lua_state);
  int return_code;
  return_code = luaL_loadfile(state->L, String_val(file));
  if(return_code != LUA_OK) {
	code_to_exception(return_code, state->L);
  }
  return_code = lua_pcall(state->L, 0, 0, 0);
  if(return_code != LUA_OK) {
	code_to_exception(return_code, state->L);
  }
  CAMLreturn(Val_unit);
}


CAMLprim value ocamlua_eval_string(value lua_state, value code) {
  CAMLparam2(lua_state, code);
  lua_State *L = OcamLuaState_tuple(lua_state)->L;
  int return_code;
  return_code = luaL_loadstring(L, String_val(code));
  if(return_code != LUA_OK) {
	code_to_exception(return_code, L);
  }
  return_code = lua_pcall(L, 0, 0, 0);
  if(return_code != LUA_OK) {
	code_to_exception(return_code, L);
  }
  CAMLreturn(Val_unit);
}

CAMLprim value ocamlua_call(value lua_state, value func, value arguments) {
  CAMLparam3(lua_state, func, arguments);
  CAMLlocal2(it, ret);
  ocamlua_state_t *s = OcamLuaState_tuple(lua_state);
  lua_State *L = s->L;
  lua_getglobal(L, String_val(func));
  int num_args = 0;
  int *c_counter = get_c_counter(L);
  it = arguments;
  while(it != Val_int(0)) {
	value_to_lua(L, Field(it, 0), s->id, c_counter);
	it = Field(it, 1);
	num_args++;
  }
  int ret_code = lua_pcall(L, num_args, 1, 0);
  if(ret_code != LUA_OK) {
	code_to_exception(ret_code, L);
  }
  int conv_error = 0;
  ret = lua_to_value_rec_check(L, &conv_error);
  if(conv_error) {
	caml_raise_constant(*caml_named_value("ocamlua-bad-value"));
  }
  CAMLreturn(ret);
}

