package erl_nif

when #config(ERL_NIF_MAJOR_VERSION, -1) == -1 || #config(ERL_NIF_MINOR_VERSION, -1) == -1 {
  #panic("ERL_NIF_MAJOR_VERSION and ERL_NIF_MINOR_VERSION must be defined!")
} else {
  ERL_NIF_MAJOR_VERSION :: #config(ERL_NIF_MAJOR_VERSION, -1)
  ERL_NIF_MINOR_VERSION :: #config(ERL_NIF_MINOR_VERSION, -1)
}

MIN_ERL_NIF_VERION_2_16 :: (ERL_NIF_MAJOR_VERSION > 2) || (ERL_NIF_MAJOR_VERSION == 2 && ERL_NIF_MINOR_VERSION >= 16)
MIN_ERL_NIF_VERION_2_17 :: (ERL_NIF_MAJOR_VERSION > 2) || (ERL_NIF_MAJOR_VERSION == 2 && ERL_NIF_MINOR_VERSION >= 17)

// FIXME: Figure out how far back we want to support
when !MIN_ERL_NIF_VERION_2_16 {
  #panic("ERL_NIF Version < 2.17 is not supported (at this time).")
}

when MIN_ERL_NIF_VERION_2_17 {
  ERL_NIF_MIN_ERTS :: "erts-14.0"
} else when MIN_ERL_NIF_VERION_2_16 {
  ERL_NIF_MIN_ERTS :: "erts-12.0"
} else {
  #panic("Unknown min ERTS version") // FIXME
}

// TODO: Figure out if this actually needs to be configurable
ERL_NIF_VM_VERSION :: #config(ERL_NIF_VM_VERSION, "beam.vanilla")