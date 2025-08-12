#include "erl_nif.h"
#include "lz4frame.h"

static ErlNifFunc nif_funcs[] = {{}};

ERL_NIF_INIT(lz4frame_nif, nif_funcs, NULL, NULL, NULL, NULL)
