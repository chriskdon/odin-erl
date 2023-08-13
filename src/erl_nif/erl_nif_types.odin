
package erl_nif

import "core:c"

// Types defined in: "erl_nif.h"

// FIXME: All the opaque structs that are not strictly pointers need to be
//        fully defined. Use `_` to hide the fields.

// FIXME: How to make structs truely opaque so a `nil` instance can't be created.

ErlNifUInt :: ErlNapiUInt
ErlNifSInt :: ErlNapiSInt

// See: https://www.erlang.org/doc/man/erl_nif#ErlNifUInt64
ErlNifUInt64 :: ErlNapiUInt64

// See: https://www.erlang.org/doc/man/erl_nif#ErlNifSInt64
ErlNifSInt64 :: ErlNapiSInt64

// See: https://www.erlang.org/doc/man/erl_nif#ERL_NIF_TERM
ERL_NIF_TERM :: ErlNifUInt

@private
enif_environment_t :: struct {}

// See: https://www.erlang.org/doc/man/erl_nif#ErlNifEnv
ErlNifEnv :: enif_environment_t

// See: https://www.erlang.org/doc/man/erl_nif#ErlNifBinary
ErlNifBinary :: struct {
  size: c.size_t,
  data: [^]c.uchar, // FIXME: Should be read-only

  /* Internals */
  _: rawptr,    // ref_bin
  _: [2]rawptr, // __spare__
}

when MIN_ERL_NIF_VERION_2_17 {
  // See: https://www.erlang.org/doc/man/erl_nif#ErlNifCharEncoding
  ErlNifCharEncoding :: enum {
    ERL_NIF_LATIN1,
    ERL_NIF_UTF8,
  }
} else {
  // See: https://www.erlang.org/doc/man/erl_nif#ErlNifCharEncoding
  ErlNifCharEncoding :: enum {
    ERL_NIF_LATIN1,
  }
}

when MIN_ERL_NIF_VERION_2_17 {
  ErlNifOnHaltCallback :: #type proc "c" (priv_data: rawptr);

  ErlNifOption :: enum {
    ERL_NIF_OPT_DELAY_HALT = 1,
    ERL_NIF_OPT_ON_HALT = 2,
  }
}

ErlNifCond :: ErlDrvCond_

when ODIN_OS == .Windows {
  // Opaque
  ErlNifEvent :: rawptr
} else {
  // Opaque
  ErlNifEvent :: c.int
}

// See: https://www.erlang.org/doc/man/erl_nif#ErlNifHash
ErlNifHash :: enum {
  ERL_NIF_INTERNAL_HASH,
  ERL_NIF_PHASH2,
}

// See: https://www.erlang.org/doc/man/erl_nif#ErlNifIOVec
ErlNifIOVec :: struct {
  iovcnt: c.int,
  size: c.size_t,
  iov: ^SysIOVec,
}

// Opaque
ErlNifMapIterator :: struct {}

ErlNifMapIteratorEntry :: enum {
  ERL_NIF_MAP_ITERATOR_FIRST = 1,
  ERL_NIF_MAP_ITERATOR_LAST = 2,
}

// See: https://www.erlang.org/doc/man/erl_nif#ErlNifMonitor
ErlNifMonitor :: ErlDrvMonitor

// Opaque
ErlNifMutex :: ErlDrvMutex_

// See: https://www.erlang.org/doc/man/erl_nif#ErlNifPid
ErlNifPid :: struct {
  _: ERL_NIF_TERM, // pid
}

// See: https://www.erlang.org/doc/man/erl_nif#ErlNifPort
ErlNifPort :: struct {
  _ : ERL_NIF_TERM, // port_id
}

// Opaque
ErlNifIOQueue :: struct {}

// Opaque
ErlNifRWLock :: struct {}

ErlNifResourceFlags :: enum {
  ERL_NIF_RT_CREATE = 1,
  ERL_NIF_RT_TAKEOVER = 2,
}

// Opaque
// See: https://www.erlang.org/doc/man/erl_nif#ErlNifResourceType
ErlNifResourceType :: struct {}

// See: https://www.erlang.org/doc/man/erl_nif#ErlNifSysInfo
ErlNifSysInfo :: ErlDrvSysInfo

ErlNifTSDKey :: c.int

ErlNifTermType :: enum {
  ErlNifTermType_ATOM = 1,
  ErlNifTermType_BITSTRING = 2,
  ErlNifTermType_FLOAT = 3,
  ErlNifTermType_FUN = 4,
  ErlNifTermType_INTEGER = 5,
  ErlNifTermType_LIST = 6,
  ErlNifTermType_MAP = 7,
  ErlNifTermType_PID = 8,
  ErlNifTermType_PORT = 9,
  ErlNifTermType_REFERENCE = 10,
  ErlNifTermType_TUPLE = 11,

  // Dummy value used in the C version to get compiler warnings about missing
  // cases in switch statements. Odin natively errors on unhandled cases so
  // this is not needed.
  // ErlNifTermType__MISSING_DEFAULT_CASE__READ_THE_MANUAL = -1,
}

ErlDrvTid_ :: struct{} // FIXME (need full definition)

ErlNifTid :: ^ErlDrvTid_ // FIXME

// TODO: Doc Name: FIXME
// See: https://www.erlang.org/doc/man/erl_nif#data-types
Erl_Nif_Thread_Opts :: ErlDrvThreadOpts

// TODO: Doc Name: FIXME
// See: https://www.erlang.org/doc/man/erl_nif#data-types
Erl_Nif_Time :: ErlNifSInt64

// See: https://www.erlang.org/doc/man/erl_nif#ErlNifTimeUnit
ErlNifTimeUnit :: enum {
  ERL_NIF_SEC    = ERTS_NAPI_SEC__,
  ERL_NIF_MSEC   = ERTS_NAPI_MSEC__,
  ERL_NIF_USEC   = ERTS_NAPI_USEC__,
  ERL_NIF_NSEC   = ERTS_NAPI_NSEC__,
}

// See: https://www.erlang.org/doc/man/erl_nif#ErlNifUniqueInteger
ErlNifUniqueInteger :: enum {
  ERL_NIF_UNIQUE_POSITIVE = (1 << 0),
  ERL_NIF_UNIQUE_MONOTONIC = (1 << 1),
}

// See: https://www.erlang.org/doc/man/erl_nif#ErlNifResourceDtor
ErlNifResourceDtor :: #type proc "c" (caller_env: ^ErlNifEnv, obj: rawptr)

// See: https://www.erlang.org/doc/man/erl_nif#ErlNifResourceStop
ErlNifResourceStop :: #type proc "c" (caller_env: ^ErlNifEnv, obj: rawptr, event: ErlNifEvent, is_direct_call: c.int)

// See: https://www.erlang.org/doc/man/erl_nif#ErlNifResourceDown
ErlNifResourceDown :: #type proc "c" (caller_env: ^ErlNifEnv, obj: rawptr, pid: ^ErlNifPid, mon: ^ErlNifMonitor);

// See: https://www.erlang.org/doc/man/erl_nif#ErlNifResourceDynCall
ErlNifResourceDynCall :: #type proc "c" (caller_env: ^ErlNifEnv, obj: rawptr, call_data:rawptr)

// See: https://www.erlang.org/doc/man/erl_nif#ErlNifResourceTypeInit
ErlNifResourceTypeInit :: struct {
  dtor: ErlNifResourceDtor,
  stop: ErlNifResourceStop,  /* at ERL_NIF_SELECT_STOP event */
  down: ErlNifResourceDown,  /* enif_monitor_process */
  members: c.int,
  dyncall: ErlNifResourceDynCall,
}

@private
ErlNifEntry :: struct {
  major: c.int,
  minor: c.int,
  name: cstring,
  num_of_funcs: i32,
  funcs: [^]ErlNifFunc,

  load: ErlNifEntry_load_proc,
  reload: ErlNifEntry_reload_proc,
  upgrade: ErlNifEntry_upgrade_proc,
  unload: ErlNifEntry_unload_proc,

  /* Added in 2.1 */
  vm_variant: cstring,

  /* Added in 2.7 */
  options: u32,   /* Unused. Can be set to 0 or 1 (dirty sched config) */

  /* Added in 2.12 */
  sizeof_ErlNifResourceTypeInit: c.size_t,

  /* Added in 2.14 */
  min_erts: cstring,
}
