package erl_nif

import "core:c"
import "core:runtime"

// TODO: Is this okay to do? I think so, the other one is static.
// Global variable to store the nif entry point data
_global_entry : ^ErlNifEntry

@export
nif_init :: proc "c" () -> ^ErlNifEntry {
  return _global_entry
}

__nif_init :: proc(
  module_name: cstring,
  nif_funcs: []ErlNifFunc,
  load: ErlNifEntry_load_proc = nil,
  reload: ErlNifEntry_reload_proc = nil,
  upgrade: ErlNifEntry_upgrade_proc = nil,
  unload: ErlNifEntry_unload_proc = nil
) {
  // TODO: Is this correct?
  // If we assign directly to global we are technically assigning a stack
  // value. it doesn't seem to get cleaned up. But I think that's because
  // this is called in
  _global_entry = new_clone(ErlNifEntry{
    major = ERL_NIF_MAJOR_VERSION,
    minor = ERL_NIF_MINOR_VERSION,

    name = module_name,

    num_of_funcs = cast(c.int) len(nif_funcs),
    funcs = raw_data(nif_funcs[:]),

    // FIXME: These probably aren't always nil
    load = load,
    reload = reload,
    upgrade = upgrade,
    unload = unload,

    /* Added in 2.1 */
    vm_variant = ERL_NIF_VM_VERSION,

    /* Added in 2.7 */
    options = 1,   /* Unused. Can be set to 0 or 1 (dirty sched config) */

    /* Added in 2.12 */
    sizeof_ErlNifResourceTypeInit = size_of(ErlNifResourceTypeInit),

    /* Added in 2.14 */
    min_erts = ERL_NIF_MIN_ERTS,
  })
}