package basic_load

import "core:fmt"
import "core:c"
import "core:runtime"

import nif "../../../src/odin_erl_nif"
import enif "../../../src/odin_erl_nif/erl_nif"

ERLANG_MODULE :: "basic_load"

@export
odin_proc : enif.ErlangFFIPointer :  proc "c" (env: ^enif.ErlNifEnv, argc: c.int, argv: [^]enif.ERL_NIF_TERM) -> enif.ERL_NIF_TERM {
  x: c.int
  if (enif.get_int(env, argv[0], &x) == 0) {
    return enif.make_badarg(env);
  }

  y: c.int
  if (enif.get_int(env, argv[1], &y) == 0) {
    return enif.make_badarg(env);
  }

  hello_str := enif.make_string(env, "Hello from Odin!", .ERL_NIF_LATIN1)
  added := enif.make_int(env, x + y)

  return enif.make_tuple(env, 2, hello_str, added)
}

nif_funcs := [?]enif.ErlNifFunc {
  {"odin_proc", 2, odin_proc, 0},
}

@(init, private)
_ :: proc() {
  nif.init(ERLANG_MODULE, nif_funcs[:])
}