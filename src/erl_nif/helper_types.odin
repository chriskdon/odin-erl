package erl_nif

import "core:c"

// This file contains unofficial types not part of defined in the `erl_nif` C library.

// FIXME: Name
// FIXME: argv: should be constant
ErlangFFIPointer :: proc "c" (env: ^ErlNifEnv, argc: c.int, argv: [^]ERL_NIF_TERM) -> ERL_NIF_TERM

// FIXME: All const array accesses should be const - not sure how to do this in odin
// Maybe there is a way to wrap the call when registering it?

ErlNifFunc :: struct {
  name: cstring,
  arity: u32,
  fptr: ErlangFFIPointer,
  flags: u32,
}

ErlNifEntry_load_proc :: #type proc "c" (env: ^ErlNifEnv, priv_data: [^]rawptr, load_info: ERL_NIF_TERM) -> i32
ErlNifEntry_reload_proc :: #type proc "c" (env: ^ErlNifEnv, priv_data: [^]rawptr, load_info: ERL_NIF_TERM) -> i32
ErlNifEntry_upgrade_proc :: #type proc "c" (env: ^ErlNifEnv, priv_data: [^]rawptr, old_priv_data: [^]rawptr, load_info: ERL_NIF_TERM) -> i32
ErlNifEntry_unload_proc :: #type proc "c" (env: ^ErlNifEnv, priv_data: rawptr) -> i32
