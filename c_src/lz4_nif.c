#include "erl_nif.h"
#include "lz4.h"

/********************/
/* Simple Functions */
/********************/

static ERL_NIF_TERM compress_default(ErlNifEnv *env, int argc,
                                     const ERL_NIF_TERM argv[]) {
  ERL_NIF_TERM src = argv[0];
  ErlNifBinary src_bin;

  if (!enif_inspect_binary(env, src, &src_bin)) {
    return enif_make_badarg(env);
  }

  const char *src_data = (char *)src_bin.data;

  ErlNifBinary dst_bin;
  int max_dst_size = LZ4_compressBound(src_bin.size);
  enif_alloc_binary(max_dst_size, &dst_bin);
  char *dst_data = (char *)dst_bin.data;

  int dst_size =
      LZ4_compress_default(src_data, dst_data, src_bin.size, max_dst_size);

  if (dst_size <= 0) {
    return enif_make_badarg(env);
  }

  enif_realloc_binary(&dst_bin, dst_size);
  ERL_NIF_TERM dst = enif_make_binary(env, &dst_bin);

  return dst;
}

static ERL_NIF_TERM decompress_safe(ErlNifEnv *env, int argc,
                                    const ERL_NIF_TERM argv[]) {
  ERL_NIF_TERM src = argv[0];
  ErlNifBinary src_bin;
  int max_dst_size;

  if ((!enif_inspect_binary(env, src, &src_bin)) ||
      (!enif_get_int(env, argv[1], &max_dst_size))) {
    return enif_make_badarg(env);
  }

  const char *src_data = (char *)src_bin.data;

  ErlNifBinary dst_bin;
  enif_alloc_binary(max_dst_size, &dst_bin);
  char *dst_data = (char *)dst_bin.data;

  int dst_size =
      LZ4_decompress_safe(src_data, dst_data, src_bin.size, max_dst_size);

  if (dst_size <= 0) {
    return enif_make_badarg(env);
  }

  enif_realloc_binary(&dst_bin, dst_size);
  ERL_NIF_TERM dst = enif_make_binary(env, &dst_bin);

  return dst;
}

static ErlNifFunc nif_funcs[] = {{"compress_default", 1, compress_default},
                                 {"decompress_safe", 2, decompress_safe}};

ERL_NIF_INIT(lz4_nif, nif_funcs, NULL, NULL, NULL, NULL);
