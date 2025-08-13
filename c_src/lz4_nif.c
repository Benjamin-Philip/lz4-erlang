#include "erl_nif.h"
#include "lz4.h"
#include "lz4hc.h"

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

static ERL_NIF_TERM compress_hc(ErlNifEnv *env, int argc,
                                const ERL_NIF_TERM argv[]) {
  ERL_NIF_TERM src = argv[0];
  ErlNifBinary src_bin;
  int compression_level;

  if ((!enif_inspect_binary(env, src, &src_bin)) ||
      !enif_get_int(env, argv[1], &compression_level) ||
      (compression_level <= 0)) {
    return enif_make_badarg(env);
  }

  const char *src_data = (char *)src_bin.data;

  ErlNifBinary dst_bin;
  int max_dst_size = LZ4_compressBound(src_bin.size);
  enif_alloc_binary(max_dst_size, &dst_bin);
  char *dst_data = (char *)dst_bin.data;

  int dst_size = LZ4_compress_HC(src_data, dst_data, src_bin.size, max_dst_size,
                                 compression_level);

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

/**********************/
/* Advanced Functions */
/**********************/

static ERL_NIF_TERM compress_bound(ErlNifEnv *env, int argc,
                                   const ERL_NIF_TERM argv[]) {
  int input_size;

  if (!enif_get_int(env, argv[0], &input_size)) {
    return enif_make_badarg(env);
  }

  int result = LZ4_compressBound(input_size);

  if (result == 0) {
    return enif_make_badarg(env);
  }

  return enif_make_int(env, result);
}

static ERL_NIF_TERM compress_fast(ErlNifEnv *env, int argc,
                                  const ERL_NIF_TERM argv[]) {
  ERL_NIF_TERM src = argv[0];
  ErlNifBinary src_bin;
  int acceleration;

  if ((!enif_inspect_binary(env, src, &src_bin)) ||
      !enif_get_int(env, argv[1], &acceleration)) {
    return enif_make_badarg(env);
  }

  const char *src_data = (char *)src_bin.data;

  ErlNifBinary dst_bin;
  int max_dst_size = LZ4_compressBound(src_bin.size);
  enif_alloc_binary(max_dst_size, &dst_bin);
  char *dst_data = (char *)dst_bin.data;

  int dst_size = LZ4_compress_fast(src_data, dst_data, src_bin.size,
                                   max_dst_size, acceleration);

  if (dst_size <= 0) {
    return enif_make_badarg(env);
  }

  enif_realloc_binary(&dst_bin, dst_size);
  ERL_NIF_TERM dst = enif_make_binary(env, &dst_bin);

  return dst;
}

static ERL_NIF_TERM compress_dest_size(ErlNifEnv *env, int argc,
                                       const ERL_NIF_TERM argv[]) {
  ERL_NIF_TERM src = argv[0];
  ErlNifBinary src_bin;
  int dst_size;

  if ((!enif_inspect_binary(env, src, &src_bin)) ||
      (!enif_get_int(env, argv[1], &dst_size)) || (dst_size <= 0)) {
    return enif_make_badarg(env);
  }

  const char *src_data = (char *)src_bin.data;
  int src_size = src_bin.size;

  ErlNifBinary dst_bin;
  enif_alloc_binary(dst_size, &dst_bin);
  char *dst_data = (char *)dst_bin.data;

  int final_dst_size =
      LZ4_compress_destSize(src_data, dst_data, &src_size, dst_size);

  if (final_dst_size == 0) {
    return enif_make_badarg(env);
  }

  enif_realloc_binary(&dst_bin, final_dst_size);
  ERL_NIF_TERM dst = enif_make_binary(env, &dst_bin);
  ERL_NIF_TERM bytes_read = enif_make_int(env, src_size);
  return enif_make_tuple2(env, dst, bytes_read);
}

static ERL_NIF_TERM compress_hc_dest_size(ErlNifEnv *env, int argc,
                                          const ERL_NIF_TERM argv[]) {
  ERL_NIF_TERM src = argv[0];
  ErlNifBinary src_bin;
  int dst_size;
  int compression_level;

  if ((!enif_inspect_binary(env, src, &src_bin)) ||
      (!enif_get_int(env, argv[1], &dst_size)) ||
      (!enif_get_int(env, argv[2], &compression_level)) || (dst_size <= 0) ||
      (compression_level <= 0)) {
    return enif_make_badarg(env);
  }

  const char *src_data = (char *)src_bin.data;
  int src_size = src_bin.size;

  ErlNifBinary dst_bin;
  enif_alloc_binary(dst_size, &dst_bin);
  char *dst_data = (char *)dst_bin.data;

  // Ensure state_hc is not NULL
  void *state_hc;
  do {
    state_hc = enif_alloc(LZ4_sizeofStateHC());
  } while (state_hc == NULL);

  int final_dst_size = LZ4_compress_HC_destSize(
      state_hc, src_data, dst_data, &src_size, dst_size, compression_level);
  enif_free(state_hc);

  if (final_dst_size == 0) {
    return enif_make_badarg(env);
  }

  enif_realloc_binary(&dst_bin, final_dst_size);
  ERL_NIF_TERM dst = enif_make_binary(env, &dst_bin);
  ERL_NIF_TERM bytes_read = enif_make_int(env, src_size);
  return enif_make_tuple2(env, dst, bytes_read);
}

static ERL_NIF_TERM decompress_safe_partial(ErlNifEnv *env, int argc,
                                            const ERL_NIF_TERM argv[]) {
  ERL_NIF_TERM src = argv[0];
  ErlNifBinary src_bin;
  int target_dst_size;

  if ((!enif_inspect_binary(env, src, &src_bin)) ||
      !enif_get_int(env, argv[1], &target_dst_size) || (target_dst_size <= 0)) {
    return enif_make_badarg(env);
  }

  const char *src_data = (char *)src_bin.data;

  ErlNifBinary dst_bin;
  enif_alloc_binary(target_dst_size, &dst_bin);
  char *dst_data = (char *)dst_bin.data;

  int dst_size = LZ4_decompress_safe_partial(src_data, dst_data, src_bin.size,
                                             target_dst_size, target_dst_size);

  if (dst_size <= 0) {
    return enif_make_badarg(env);
  }

  enif_realloc_binary(&dst_bin, dst_size);
  ERL_NIF_TERM dst = enif_make_binary(env, &dst_bin);

  return dst;
}

/******************/
/* NIF Management */
/******************/

static ErlNifFunc nif_funcs[] = {
    // Simple Functions

    {"compress_default", 1, compress_default},
    {"compress_hc", 2, compress_hc},
    {"decompress_safe", 2, decompress_safe},

    // Advanced Functions
    {"compress_bound", 1, compress_bound},
    {"compress_fast", 2, compress_fast},
    {"compress_dest_size", 2, compress_dest_size},
    {"compress_hc_dest_size", 3, compress_hc_dest_size},
    {"decompress_safe_partial", 2, decompress_safe_partial}};

ERL_NIF_INIT(lz4_nif, nif_funcs, NULL, NULL, NULL, NULL);
