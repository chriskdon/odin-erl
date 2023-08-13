package erl_nif

import "core:c"
import "core:c/libc"

// TODO: Review https://www.erlang.org/doc/man/erl_nif

/* TODO MANUAL ADD
#if SIZEOF_LONG != 8
ERL_NIF_API_FUNC_DECL(int,enif_get_int64,(ErlNifEnv*, ERL_NIF_TERM term, ErlNifSInt64* ip));
ERL_NIF_API_FUNC_DECL(int,enif_get_uint64,(ErlNifEnv*, ERL_NIF_TERM term, ErlNifUInt64* ip));
ERL_NIF_API_FUNC_DECL(ERL_NIF_TERM,enif_make_int64,(ErlNifEnv*, ErlNifSInt64));
ERL_NIF_API_FUNC_DECL(ERL_NIF_TERM,enif_make_uint64,(ErlNifEnv*, ErlNifUInt64));
#endif

ERL_NIF_API_FUNC_DECL(ErlNifMutex*,enif_mutex_create,(char *name) ERL_NAPI_ATTR_MALLOC_D(enif_mutex_destroy,1));
ERL_NIF_API_FUNC_DECL(ErlNifCond*,enif_cond_create,(char *name) ERL_NAPI_ATTR_MALLOC_D(enif_cond_destroy,1));
ERL_NIF_API_FUNC_DECL(ErlNifRWLock*,enif_rwlock_create,(char *name) ERL_NAPI_ATTR_MALLOC_D(enif_rwlock_destroy,1));
ERL_NIF_API_FUNC_DECL(ErlNifThreadOpts*,enif_thread_opts_create,(char *name) ERL_NAPI_ATTR_MALLOC_D(enif_thread_opts_destroy,1));
ERL_NIF_API_FUNC_DECL(void*,enif_realloc,(void* ptr, size_t size) ERL_NAPI_ATTR_ALLOC_SIZE(2));
ERL_NIF_API_FUNC_DECL(void *, enif_alloc_resource, (ErlNifResourceType *type, size_t size) ERL_NAPI_ATTR_MALLOC_US(2));
ERL_NIF_API_FUNC_DECL(ErlNifIOQueue *,enif_ioq_create,(ErlNifIOQueueOpts opts) ERL_NAPI_ATTR_MALLOC_D(enif_ioq_destroy,1));
ERL_NIF_API_FUNC_DECL(void*,enif_alloc,(size_t size) ERL_NAPI_ATTR_MALLOC_USD(1,enif_free,1));
ERL_NIF_API_FUNC_DECL(ErlNifEnv*,enif_alloc_env,(void) ERL_NAPI_ATTR_WUR);
*/

@(default_calling_convention = "c")
@(link_prefix = "enif_")
foreign {
  // See: https://www.erlang.org/doc/man/erl_nif#enif_priv_data
  priv_data :: proc (env: ^ErlNifEnv) -> rawptr ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_free
  free :: proc (ptr: rawptr)  ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_is_atom
  is_atom :: proc (env: ^ErlNifEnv, term: ERL_NIF_TERM) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_is_binary
  is_binary :: proc (env: ^ErlNifEnv, term: ERL_NIF_TERM) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_is_ref
  is_ref :: proc (env: ^ErlNifEnv, term: ERL_NIF_TERM) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_inspect_binary
  inspect_binary :: proc (env: ^ErlNifEnv, bin_term: ERL_NIF_TERM, bin: ^ErlNifBinary) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_alloc_binary
  alloc_binary :: proc (size: c.size_t, bin: ^ErlNifBinary) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_realloc_binary
  realloc_binary :: proc (bin: ^ErlNifBinary, size: c.size_t) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_release_binary
  release_binary :: proc (bin: ^ErlNifBinary)  ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_get_int
  get_int :: proc (env: ^ErlNifEnv, term: ERL_NIF_TERM, ip: ^c.int) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_get_ulong
  get_ulong :: proc (env: ^ErlNifEnv, term: ERL_NIF_TERM, ip: ^c.ulong) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_get_double
  get_double :: proc (env: ^ErlNifEnv, term: ERL_NIF_TERM, dp: ^c.double) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_get_list_cell
  get_list_cell :: proc (env: ^ErlNifEnv, term: ERL_NIF_TERM, head: ^ERL_NIF_TERM, tail: ^ERL_NIF_TERM) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_get_tuple
  get_tuple :: proc (env: ^ErlNifEnv, tpl: ERL_NIF_TERM, arity: ^c.int, array: [^]^ERL_NIF_TERM) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_is_identical
  is_identical :: proc (lhs: ERL_NIF_TERM, rhs: ERL_NIF_TERM) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_compare
  compare :: proc (lhs: ERL_NIF_TERM, rhs: ERL_NIF_TERM) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_make_binary
  make_binary :: proc (env: ^ErlNifEnv, bin: ^ErlNifBinary) -> ERL_NIF_TERM ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_make_badarg
  make_badarg :: proc (env: ^ErlNifEnv) -> ERL_NIF_TERM ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_make_int
  make_int :: proc (env: ^ErlNifEnv, i: c.int) -> ERL_NIF_TERM ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_make_ulong
  make_ulong :: proc (env: ^ErlNifEnv, i: c.ulong) -> ERL_NIF_TERM ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_make_double
  make_double :: proc (env: ^ErlNifEnv, d: c.double) -> ERL_NIF_TERM ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_make_atom
  make_atom :: proc (env: ^ErlNifEnv, name: cstring) -> ERL_NIF_TERM ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_make_existing_atom
  make_existing_atom :: proc (env: ^ErlNifEnv, name: cstring, atom: ^ERL_NIF_TERM, char_encoding: ErlNifCharEncoding) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_make_tuple
  make_tuple :: proc (env: ^ErlNifEnv, cnt: c.uint, #c_vararg args: ..ERL_NIF_TERM) -> ERL_NIF_TERM ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_make_list
  make_list :: proc (env: ^ErlNifEnv, cnt: c.uint, #c_vararg args: ..ERL_NIF_TERM) -> ERL_NIF_TERM ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_make_list_cell
  make_list_cell :: proc (env: ^ErlNifEnv, car: ERL_NIF_TERM, cdr: ERL_NIF_TERM) -> ERL_NIF_TERM ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_make_string
  make_string :: proc (env: ^ErlNifEnv, string: cstring, char_encoding: ErlNifCharEncoding) -> ERL_NIF_TERM ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_make_ref
  make_ref :: proc (env: ^ErlNifEnv) -> ERL_NIF_TERM ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_mutex_destroy
  mutex_destroy :: proc (mtx: ^ErlNifMutex)  ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_mutex_trylock
  mutex_trylock :: proc (mtx: ^ErlNifMutex) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_mutex_lock
  mutex_lock :: proc (mtx: ^ErlNifMutex)  ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_mutex_unlock
  mutex_unlock :: proc (mtx: ^ErlNifMutex)  ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_cond_destroy
  cond_destroy :: proc (cnd: ^ErlNifCond)  ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_cond_signal
  cond_signal :: proc (cnd: ^ErlNifCond)  ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_cond_broadcast
  cond_broadcast :: proc (cnd: ^ErlNifCond)  ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_cond_wait
  cond_wait :: proc (cnd: ^ErlNifCond, mtx: ^ErlNifMutex)  ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_rwlock_destroy
  rwlock_destroy :: proc (rwlck: ^ErlNifRWLock)  ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_rwlock_tryrlock
  rwlock_tryrlock :: proc (rwlck: ^ErlNifRWLock) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_rwlock_rlock
  rwlock_rlock :: proc (rwlck: ^ErlNifRWLock)  ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_rwlock_runlock
  rwlock_runlock :: proc (rwlck: ^ErlNifRWLock)  ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_rwlock_tryrwlock
  rwlock_tryrwlock :: proc (rwlck: ^ErlNifRWLock) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_rwlock_rwlock
  rwlock_rwlock :: proc (rwlck: ^ErlNifRWLock)  ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_rwlock_rwunlock
  rwlock_rwunlock :: proc (rwlck: ^ErlNifRWLock)  ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_tsd_key_create
  tsd_key_create :: proc (name: [^]c.char, key: ^ErlNifTSDKey) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_tsd_key_destroy
  tsd_key_destroy :: proc (key: ErlNifTSDKey)  ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_tsd_set
  tsd_set :: proc (key: ErlNifTSDKey, data: rawptr)  ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_tsd_get
  tsd_get :: proc (key: ErlNifTSDKey) -> rawptr ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_thread_opts_destroy
  thread_opts_destroy :: proc (opts: ^Erl_Nif_Thread_Opts)  ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_thread_create
  thread_create :: proc (name: [^]c.char, tid: ^ErlNifTid, func: proc (FIXME_arg1: rawptr) -> rawptr, args: rawptr, opts: ^Erl_Nif_Thread_Opts) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_thread_self
  thread_self :: proc () -> ErlNifTid ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_equal_tids
  equal_tids :: proc (tid1: ErlNifTid, tid2: ErlNifTid) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_thread_exit
  thread_exit :: proc (resp: rawptr)  ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_thread_join
  thread_join :: proc (thread_id: ErlNifTid, respp: ^rawptr) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_system_info
  system_info :: proc (sip: ^ErlNifSysInfo, si_size: c.size_t)  ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_fprintf
  fprintf :: proc (filep: ^libc.FILE, format: cstring, #c_vararg args: ..any) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_inspect_iolist_as_binary
  inspect_iolist_as_binary :: proc (env: ^ErlNifEnv, term: ERL_NIF_TERM, bin: ^ErlNifBinary) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_make_sub_binary
  make_sub_binary :: proc (env: ^ErlNifEnv, bin_term: ERL_NIF_TERM, pos: c.size_t, size: c.size_t) -> ERL_NIF_TERM ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_get_string
  get_string :: proc (env: ^ErlNifEnv, list: ERL_NIF_TERM, buf: [^]c.char, len: c.uint, char_encoding: ErlNifCharEncoding) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_get_atom
  get_atom :: proc (env: ^ErlNifEnv, atom: ERL_NIF_TERM, buf: [^]c.char, len: c.uint, char_encoding: ErlNifCharEncoding) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_is_fun
  is_fun :: proc (env: ^ErlNifEnv, term: ERL_NIF_TERM) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_is_pid
  is_pid :: proc (env: ^ErlNifEnv, term: ERL_NIF_TERM) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_is_port
  is_port :: proc (env: ^ErlNifEnv, term: ERL_NIF_TERM) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_get_uint
  get_uint :: proc (env: ^ErlNifEnv, term: ERL_NIF_TERM, ip: ^c.uint) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_get_long
  get_long :: proc (env: ^ErlNifEnv, term: ERL_NIF_TERM, ip: ^c.long) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_make_uint
  make_uint :: proc (env: ^ErlNifEnv, i: c.uint) -> ERL_NIF_TERM ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_make_long
  make_long :: proc (env: ^ErlNifEnv, i: c.long) -> ERL_NIF_TERM ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_make_tuple_from_array
  make_tuple_from_array :: proc (env: ^ErlNifEnv, arr: [^]ERL_NIF_TERM, cnt: c.uint) -> ERL_NIF_TERM ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_make_list_from_array
  make_list_from_array :: proc (env: ^ErlNifEnv, arr: [^]ERL_NIF_TERM, cnt: c.uint) -> ERL_NIF_TERM ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_is_empty_list
  is_empty_list :: proc (env: ^ErlNifEnv, term: ERL_NIF_TERM) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_open_resource_type
  open_resource_type :: proc (env: ^ErlNifEnv, module_str: cstring, name_str: cstring, dtor: proc (FIXME_arg: ^ErlNifEnv, FIXME_arg2: rawptr) , flags: ErlNifResourceFlags, tried: ^ErlNifResourceFlags) -> ^ErlNifResourceType ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_release_resource
  release_resource :: proc (obj: rawptr)  ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_make_resource
  make_resource :: proc (env: ^ErlNifEnv, obj: rawptr) -> ERL_NIF_TERM ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_get_resource
  get_resource :: proc (env: ^ErlNifEnv, term: ERL_NIF_TERM, type: ^ErlNifResourceType, objp: ^rawptr) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_sizeof_resource
  sizeof_resource :: proc (obj: rawptr) -> c.size_t ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_make_new_binary
  make_new_binary :: proc (env: ^ErlNifEnv, size: c.size_t, termp: ^ERL_NIF_TERM) -> ^c.uchar ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_is_list
  is_list :: proc (env: ^ErlNifEnv, term: ERL_NIF_TERM) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_is_tuple
  is_tuple :: proc (env: ^ErlNifEnv, term: ERL_NIF_TERM) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_get_atom_length
  get_atom_length :: proc (env: ^ErlNifEnv, atom: ERL_NIF_TERM, len: ^c.uint, char_encoding: ErlNifCharEncoding) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_get_list_length
  get_list_length :: proc (env: ^ErlNifEnv, term: ERL_NIF_TERM, len: ^c.uint) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif# enif_make_atom_len
  make_atom_len :: proc (env: ^ErlNifEnv, name: cstring, len: c.size_t) -> ERL_NIF_TERM ---

  // See: https://www.erlang.org/doc/man/erl_nif# enif_make_existing_atom_len
  make_existing_atom_len :: proc (env: ^ErlNifEnv, name: cstring, len: c.size_t, atom: ^ERL_NIF_TERM, char_encoding: ErlNifCharEncoding) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_make_string_len
  make_string_len :: proc (env: ^ErlNifEnv, string: cstring, len: c.size_t, char_encoding: ErlNifCharEncoding) -> ERL_NIF_TERM ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_free_env
  free_env :: proc (env: ^ErlNifEnv)  ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_clear_env
  clear_env :: proc (env: ^ErlNifEnv)  ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_send
  send :: proc (env: ^ErlNifEnv, #by_ptr to_pid: ErlNifPid, msg_env: ^ErlNifEnv, msg: ERL_NIF_TERM) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_make_copy
  make_copy :: proc (dst_env: ^ErlNifEnv, src_term: ERL_NIF_TERM) -> ERL_NIF_TERM ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_self
  self :: proc (caller_env: ^ErlNifEnv, pid: ^ErlNifPid) -> ^ErlNifPid ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_get_local_pid
  get_local_pid :: proc (env: ^ErlNifEnv, term: ERL_NIF_TERM, pid: ^ErlNifPid) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_keep_resource
  keep_resource :: proc (obj: rawptr)  ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_make_resource_binary
  make_resource_binary :: proc (env: ^ErlNifEnv, obj: rawptr, data: any, size: c.size_t) -> ERL_NIF_TERM ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_is_exception
  is_exception :: proc (env: ^ErlNifEnv, term: ERL_NIF_TERM) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_make_reverse_list
  make_reverse_list :: proc (env: ^ErlNifEnv, term: ERL_NIF_TERM, list: ^ERL_NIF_TERM) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_is_number
  is_number :: proc (env: ^ErlNifEnv, term: ERL_NIF_TERM) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_dlopen
  dlopen :: proc (lib: cstring, err_handler: proc (FIXME_arg: rawptr, FIXME_arg2: cstring) , err_arg: rawptr) -> rawptr ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_dlsym
  dlsym :: proc (handle: rawptr, symbol: cstring, err_handler: proc (FIXME_arg: rawptr, FIXME_arg2: cstring) , err_arg: rawptr) -> rawptr ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_consume_timeslice
  consume_timeslice :: proc (env: ^ErlNifEnv, percent: c.int) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif# enif_is_map
  is_map :: proc (env: ^ErlNifEnv, term: ERL_NIF_TERM) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif# enif_get_map_size
  get_map_size :: proc (env: ^ErlNifEnv, term: ERL_NIF_TERM, size: ^c.size_t) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif# enif_make_new_map
  make_new_map :: proc (env: ^ErlNifEnv) -> ERL_NIF_TERM ---

  // See: https://www.erlang.org/doc/man/erl_nif# enif_make_map_put
  make_map_put :: proc (env: ^ErlNifEnv, map_in: ERL_NIF_TERM, key: ERL_NIF_TERM, value: ERL_NIF_TERM, map_out: ^ERL_NIF_TERM) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif# enif_get_map_value
  get_map_value :: proc (env: ^ErlNifEnv, emap: ERL_NIF_TERM, key: ERL_NIF_TERM, value: ^ERL_NIF_TERM) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif# enif_make_map_update
  make_map_update :: proc (env: ^ErlNifEnv, map_in: ERL_NIF_TERM, key: ERL_NIF_TERM, value: ERL_NIF_TERM, map_out: ^ERL_NIF_TERM) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif# enif_make_map_remove
  make_map_remove :: proc (env: ^ErlNifEnv, map_in: ERL_NIF_TERM, key: ERL_NIF_TERM, map_out: ^ERL_NIF_TERM) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif# enif_map_iterator_create
  map_iterator_create :: proc (env: ^ErlNifEnv, emap: ERL_NIF_TERM, iter: ^ErlNifMapIterator, entry: ErlNifMapIteratorEntry) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif# enif_map_iterator_destroy
  map_iterator_destroy :: proc (env: ^ErlNifEnv, iter: ^ErlNifMapIterator)  ---

  // See: https://www.erlang.org/doc/man/erl_nif# enif_map_iterator_is_head
  map_iterator_is_head :: proc (env: ^ErlNifEnv, iter: ^ErlNifMapIterator) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif# enif_map_iterator_is_tail
  map_iterator_is_tail :: proc (env: ^ErlNifEnv, iter: ^ErlNifMapIterator) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif# enif_map_iterator_next
  map_iterator_next :: proc (env: ^ErlNifEnv, iter: ^ErlNifMapIterator) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif# enif_map_iterator_prev
  map_iterator_prev :: proc (env: ^ErlNifEnv, iter: ^ErlNifMapIterator) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif# enif_map_iterator_get_pair
  map_iterator_get_pair :: proc (env: ^ErlNifEnv, iter: ^ErlNifMapIterator, key: ^ERL_NIF_TERM, value: ^ERL_NIF_TERM) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_schedule_nif
  schedule_nif :: proc (env: ^ErlNifEnv, FIXME_STR: cstring, FIXME_INT: c.int, FIXME_PROC: proc (FIXME_arg: ^ErlNifEnv, FIXME_arg2: c.int, FIXME_arg3: [^]ERL_NIF_TERM) -> ERL_NIF_TERM, FIXME_INT1: c.int, arr: [^]ERL_NIF_TERM) -> ERL_NIF_TERM ---

  // See: https://www.erlang.org/doc/man/erl_nif# enif_has_pending_exception
  has_pending_exception :: proc (env: ^ErlNifEnv, reason: ^ERL_NIF_TERM) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif# enif_raise_exception
  raise_exception :: proc (env: ^ErlNifEnv, reason: ERL_NIF_TERM) -> ERL_NIF_TERM ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_getenv
  getenv :: proc (key: cstring, value: [^]c.char, value_size: ^c.size_t) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif# enif_monotonic_time
  monotonic_time :: proc (time_unit: ErlNifTimeUnit) -> Erl_Nif_Time ---

  // See: https://www.erlang.org/doc/man/erl_nif# enif_time_offset
  time_offset :: proc (time_unit: ErlNifTimeUnit) -> Erl_Nif_Time ---

  // See: https://www.erlang.org/doc/man/erl_nif# enif_convert_time_unit
  convert_time_unit :: proc (time: Erl_Nif_Time, FIXME_1: ErlNifTimeUnit, FIXME_2: ErlNifTimeUnit) -> Erl_Nif_Time ---

  // See: https://www.erlang.org/doc/man/erl_nif# enif_now_time
  now_time :: proc (env: ^ErlNifEnv) -> ERL_NIF_TERM ---

  // See: https://www.erlang.org/doc/man/erl_nif# enif_cpu_time
  cpu_time :: proc (env: ^ErlNifEnv) -> ERL_NIF_TERM ---

  // See: https://www.erlang.org/doc/man/erl_nif# enif_make_unique_integer
  make_unique_integer :: proc (env: ^ErlNifEnv, properties: ErlNifUniqueInteger) -> ERL_NIF_TERM ---

  // See: https://www.erlang.org/doc/man/erl_nif# enif_is_current_process_alive
  is_current_process_alive :: proc (env: ^ErlNifEnv) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif# enif_is_process_alive
  is_process_alive :: proc (env: ^ErlNifEnv, pid: ^ErlNifPid) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif# enif_is_port_alive
  is_port_alive :: proc (env: ^ErlNifEnv, port_id: ^ErlNifPort) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif# enif_get_local_port
  get_local_port :: proc (env: ^ErlNifEnv, term: ERL_NIF_TERM, port_id: ^ErlNifPort) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif# enif_term_to_binary
  term_to_binary :: proc (env: ^ErlNifEnv, term: ERL_NIF_TERM, bin: ^ErlNifBinary) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif# enif_binary_to_term
  binary_to_term :: proc (env: ^ErlNifEnv, #by_ptr data: c.uchar, sz: c.size_t, term: ^ERL_NIF_TERM, opts: c.uint) -> c.size_t ---

  // See: https://www.erlang.org/doc/man/erl_nif# enif_port_command
  port_command :: proc (env: ^ErlNifEnv, #by_ptr to_port: ErlNifPort, msg_env: ^ErlNifEnv, msg: ERL_NIF_TERM) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_thread_type
  thread_type :: proc () -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_snprintf
  snprintf :: proc (buffer: [^]c.char, size: c.size_t, format: cstring, #c_vararg args: ..any) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_select
  select :: proc (env: ^ErlNifEnv, e: ErlNifEvent, flags: ErlNifSelectFlags, obj: rawptr, #by_ptr pid: ErlNifPid, ref: ERL_NIF_TERM) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_open_resource_type_x
  open_resource_type_x :: proc (env: ^ErlNifEnv, name_str: cstring, #by_ptr resource_type_init: ErlNifResourceTypeInit, flags: ErlNifResourceFlags, tried: ^ErlNifResourceFlags) -> ^ErlNifResourceType ---

  // See: https://www.erlang.org/doc/man/erl_nif# enif_monitor_process
  monitor_process :: proc (env: ^ErlNifEnv, obj: rawptr, #by_ptr pid: ErlNifPid, monitor: ^ErlNifMonitor) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif# enif_demonitor_process
  demonitor_process :: proc (env: ^ErlNifEnv, obj: rawptr, #by_ptr monitor: ErlNifMonitor) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif# enif_compare_monitors
  compare_monitors :: proc (#by_ptr monitor_1: ErlNifMonitor, #by_ptr monitor_2: ErlNifMonitor) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_hash
  hash :: proc (type: ErlNifHash, term: ERL_NIF_TERM, salt: ErlNifUInt64) -> ErlNifUInt64 ---

  // See: https://www.erlang.org/doc/man/erl_nif# enif_whereis_pid
  whereis_pid :: proc (env: ^ErlNifEnv, name: ERL_NIF_TERM, pid: ^ErlNifPid) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif# enif_whereis_port
  whereis_port :: proc (env: ^ErlNifEnv, name: ERL_NIF_TERM, port: ^ErlNifPort) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_ioq_destroy
  ioq_destroy :: proc (q: ^ErlNifIOQueue)  ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_ioq_enq_binary
  ioq_enq_binary :: proc (q: ^ErlNifIOQueue, bin: ^ErlNifBinary, skip: c.size_t) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_ioq_enqv
  ioq_enqv :: proc (q: ^ErlNifIOQueue, iov: ^ErlNifIOVec, skip: c.size_t) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_ioq_size
  ioq_size :: proc (q: ^ErlNifIOQueue) -> c.size_t ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_ioq_deq
  ioq_deq :: proc (q: ^ErlNifIOQueue, count: c.size_t, size: ^c.size_t) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_ioq_peek
  ioq_peek :: proc (q: ^ErlNifIOQueue, iovlen: ^c.int) -> ^SysIOVec ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_inspect_iovec
  inspect_iovec :: proc (env: ^ErlNifEnv, max_length: c.size_t, iovec_term: ERL_NIF_TERM, tail: ^ERL_NIF_TERM, iovec: ^^ErlNifIOVec) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_free_iovec
  free_iovec :: proc (iov: ^ErlNifIOVec)  ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_ioq_peek_head
  ioq_peek_head :: proc (env: ^ErlNifEnv, q: ^ErlNifIOQueue, size: ^c.size_t, head: ^ERL_NIF_TERM) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_mutex_name
  mutex_name :: proc (mutex: ^ErlNifMutex) -> [^]c.char ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_cond_name
  cond_name :: proc (cond: ^ErlNifCond) -> [^]c.char ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_rwlock_name
  rwlock_name :: proc (rw_lock: ^ErlNifRWLock) -> [^]c.char ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_thread_name
  thread_name :: proc (thread_id: ErlNifTid) -> [^]c.char ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_vfprintf
  vfprintf :: proc (FIXME_FILE: ^libc.FILE, fmt: cstring, FIXME_VA_LIST: libc.va_list) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_vsnprintf
  vsnprintf :: proc (FIXME_STR: [^]c.char, FIXME_SIZE_T: c.size_t, fmt: cstring, FIXME_VA_LIST: libc.va_list) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_make_map_from_arrays
  make_map_from_arrays :: proc (env: ^ErlNifEnv, keys: [^]ERL_NIF_TERM, values: [^]ERL_NIF_TERM, cnt: c.size_t, map_out: ^ERL_NIF_TERM) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_select_x
  select_x :: proc (env: ^ErlNifEnv, e: ErlNifEvent, flags: ErlNifSelectFlags, obj: rawptr, #by_ptr pid: ErlNifPid, msg: ERL_NIF_TERM, msg_env: ^ErlNifEnv) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_make_monitor_term
  make_monitor_term :: proc (env: ^ErlNifEnv, #by_ptr monitor: ErlNifMonitor) -> ERL_NIF_TERM ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_set_pid_undefined
  set_pid_undefined :: proc (pid: ^ErlNifPid)  ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_is_pid_undefined
  is_pid_undefined :: proc (#by_ptr pid: ErlNifPid) -> c.int ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_term_type
  term_type :: proc (env: ^ErlNifEnv, term: ERL_NIF_TERM) -> ErlNifTermType ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_init_resource_type
  init_resource_type :: proc (env: ^ErlNifEnv, name_str: cstring, #by_ptr resource_type_init: ErlNifResourceTypeInit, flags: ErlNifResourceFlags, tried: ^ErlNifResourceFlags) -> ^ErlNifResourceType ---

  // See: https://www.erlang.org/doc/man/erl_nif#enif_dynamic_resource_call
  dynamic_resource_call :: proc (env: ^ErlNifEnv, mod: ERL_NIF_TERM, name: ERL_NIF_TERM, rsrc: ERL_NIF_TERM, call_data: rawptr) -> c.int ---
}
