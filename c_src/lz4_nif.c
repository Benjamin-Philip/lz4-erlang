#include "erl_nif.h"
#include "lz4.h"

static ErlNifFunc nif_funcs[] = {{}};

ERL_NIF_INIT(lz4_nif, nif_funcs, NULL, NULL, NULL, NULL);
