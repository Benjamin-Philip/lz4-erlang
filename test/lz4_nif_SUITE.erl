-module(lz4_nif_SUITE).

-export([all/0]).
-export([compress_default/1, decompress_safe/1]).
-export([compress_bound/1, compress_fast/1, compress_dest_size/1, decompress_safe_partial/1]).
-export([doc_test/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

all() ->
    [
        %% Simple Functions
        compress_default,
        decompress_safe,

        %% Advanced Functions
        compress_bound,
        compress_fast,
        compress_dest_size,
        decompress_safe_partial,

        %% DocTests
        doc_test
    ].

%%%%%%%%%%%%%%%%%%%%%%
%% Simple Functions %%
%%%%%%%%%%%%%%%%%%%%%%

compress_default(_Config) ->
    Bin = gen_bin(),
    CompressedBin = lz4_nif:compress_default(Bin),
    ?assert(is_binary(CompressedBin)),
    ?assertNotEqual(Bin, CompressedBin),
    % check determininism to check allocated size
    ?assertEqual(CompressedBin, lz4_nif:compress_default(Bin)),

    ?assertError(badarg, lz4_nif:compress_default(0)).

decompress_safe(_Config) ->
    Bin = gen_bin(),
    CompressedBin = lz4_nif:compress_default(Bin),
    BinSize = byte_size(Bin),
    ?assertEqual(Bin, lz4_nif:decompress_safe(CompressedBin, BinSize)),
    ?assertEqual(Bin, lz4_nif:decompress_safe(CompressedBin, BinSize + 1)),

    ?assertError(badarg, lz4_nif:decompress_safe(CompressedBin, BinSize - 1)),
    ?assertError(badarg, lz4_nif:decompress_safe(0, 0)),
    ?assertError(badarg, lz4_nif:decompress_safe(gen_bin(), byte_size(gen_bin()))).

%%%%%%%%%%%%%%%%%%%%%%%%
%% Advanced Functions %%
%%%%%%%%%%%%%%%%%%%%%%%%

compress_bound(_Config) ->
    ?assertEqual(26, lz4_nif:compress_bound(10)),
    ?assertError(badarg, lz4_nif:compress_bound(-1)),
    ?assertError(badarg, lz4_nif:compress_bound(foo)).

compress_fast(_Config) ->
    Bin = gen_bin(),
    CompressedBin = lz4_nif:compress_fast(Bin, 2),
    ?assertEqual(Bin, lz4_nif:decompress_safe(CompressedBin, byte_size(Bin))),

    LargeBin = <<X || X <- lists:duplicate(10, Bin)>>,
    ?assertNotEqual(lz4_nif:compress_fast(LargeBin, 1), lz4_nif:compress_fast(LargeBin, 6000)),

    ?assertError(badarg, lz4_nif:compress_fast(foo, 2)),
    ?assertError(badarg, lz4_nif:compress_fast(Bin, foo)).

compress_dest_size(_Config) ->
    Bin = gen_bin(),
    {CompressBin, BytesRead} = lz4_nif:compress_dest_size(Bin, 3),
    ?assertEqual(binary:part(Bin, 0, BytesRead), lz4_nif:decompress_safe(CompressBin, BytesRead)),

    ?assertError(badarg, lz4_nif:compress_dest_size(Bin, -2)),
    ?assertError(badarg, lz4_nif:compress_dest_size(Bin, foo)),
    ?assertError(badarg, lz4_nif:compress_dest_size(foo, 3)).

decompress_safe_partial(_Config) ->
    Bin = gen_bin(),
    CompressedBin = lz4_nif:compress_default(Bin),
    BinSize = byte_size(Bin),
    ?assertEqual(Bin, lz4_nif:decompress_safe_partial(CompressedBin, BinSize)),
    ?assertEqual(Bin, lz4_nif:decompress_safe_partial(CompressedBin, BinSize + 1)),
    ?assertEqual(
        binary:part(Bin, 0, BinSize - 3),
        lz4_nif:decompress_safe_partial(CompressedBin, BinSize - 3)
    ),

    ?assertError(badarg, lz4_nif:decompress_safe(Bin, 0)),
    ?assertError(badarg, lz4_nif:decompress_safe(foo, BinSize)),
    ?assertError(badarg, lz4_nif:decompress_safe(gen_bin(), byte_size(gen_bin()))).

%%%%%%%%%%%%%%
%% DocTests %%
%%%%%%%%%%%%%%

doc_test(_Config) ->
    shell_docs:test(lz4_nif, []).

%%%%%%%%%%%%%
%% Helpers %%
%%%%%%%%%%%%%

gen_bin() ->
    gen_bin(16).
gen_bin(N) ->
    crypto:strong_rand_bytes(N).
