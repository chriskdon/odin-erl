//+private
package erl_nif

// Types defined in: "erl_drv_nif.h"

// `erl_drv` is not fully supported. Until then all definitions should remain
// private.

import "core:c"

ERTS_NAPI_SEC__  ::	0
ERTS_NAPI_MSEC__ ::	1
ERTS_NAPI_USEC__ ::	2
ERTS_NAPI_NSEC__ ::	3

when size_of(c.long) == 8 {
  ErlNapiUInt64 :: c.ulong
  ErlNapiSInt64 :: c.long
} else when size_of(c.longlong) == 8 {
  ErlNapiUInt64 :: c.ulonglong
  ErlNapiSInt64 :: c.longlong
} else {
  #panic("No 64-bit integer type")
}

// FIXME: Do these need to be exposed?
// ERL_NAPI_SINT64_MAX__ :: 9_223_372_036_854_775_807
// ERL_NAPI_SINT64_MIN__ :: (-ERL_NAPI_SINT64_MAX__ - 1)

when size_of(rawptr) == 8 {
  ErlNapiUInt :: ErlNapiUInt64
  ErlNapiSInt :: ErlNapiSInt64
} else when size_of(rawptr) == 4 {
  when size_of(c.long) == size_of(rawptr) {
    ErlNapiUInt :: c.ulong
    ErlNapiSInt :: c.long
  } else when size_of(c.int) == size_of(rawptr) {
    ErlNapiUInt :: c.uint
    ErlNapiSInt :: c.int
  } else {
    #panic("No 32-bit integer type")
  }
} else {
  #panic("Arch not supported")
}

// Opaque
// See: https://www.erlang.org/doc/man/erl_driver#data-types
ErlDrvMonitor :: struct {
  _: [size_of(rawptr) * 4]c.uchar, // data
}

// See: https://www.erlang.org/doc/man/erl_driver#ErlDrvSysInfo
ErlDrvSysInfo :: struct {
  driver_major_version: c.int,
  driver_minor_version: c.int,
  erts_version: [^]c.char,
  otp_release: [^]c.char,
  thread_support: c.int,
  smp_support: c.int,
  async_threads: c.int,
  scheduler_threads: c.int,
  nif_major_version: c.int,
  nif_minor_version: c.int,
  dirty_scheduler_support: c.int,
}

// https://www.erlang.org/doc/man/erl_driver#ErlDrvThreadOpts
ErlDrvThreadOpts :: struct {
  suggested_stack_size: c.int
}

when ODIN_OS == .Windows {
  // See: https://www.erlang.org/doc/man/erl_nif#SysIOVec
  SysIOVec :: struct {
    iov_len: c.ulong,
    iov_base: ^c.char,
  }
} else {
  // See: https://www.erlang.org/doc/man/erl_nif#SysIOVec
  SysIOVec :: struct {
    iov_base: ^c.char,
    iov_len: c.size_t,
  }
}

ErlNifSelectFlags :: enum {
  ERL_NIF_SELECT_READ       = (1 << 0),
  ERL_NIF_SELECT_WRITE      = (1 << 1),
  ERL_NIF_SELECT_STOP       = (1 << 2),
  ERL_NIF_SELECT_CANCEL     = (1 << 3),
  ERL_NIF_SELECT_CUSTOM_MSG = (1 << 4),
  ERL_NIF_SELECT_ERROR      = (1 << 5),
}

ErlDrvMutex_ :: struct {} // Opaque
ErlDrvCond_ :: struct {} // Opaque