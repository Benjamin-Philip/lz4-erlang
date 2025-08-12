-module(lz4_nif_SUITE).

-export([all/0]).
-export([compress_default/1, decompress_safe/1]).
-export([doc_test/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

all() ->
    [
        %% Simple Functions
        compress_default,
        decompress_safe,

        %% DocTests
        doc_test
    ].

%%%%%%%%%%%%%%%%%%%%%%
%% Simple Functions %%
%%%%%%%%%%%%%%%%%%%%%%

compress_default(_Config) ->
    Bin = gen_bin(),
    CompressedBin = lz4_nif:compress_default(Bin),
    ?assertNotEqual(Bin, CompressedBin),
    % check determininism
    ?assertEqual(CompressedBin, lz4_nif:compress_default(Bin)),
    ?assert(is_binary(CompressedBin)),

    ?assertError(badarg, lz4_nif:compress_default(0)).

decompress_safe(_Config) ->
    Bin = gen_bin(),
    CompressedBin = lz4_nif:compress_default(Bin),
    CompressedBinSize = byte_size(Bin),
    ?assertEqual(Bin, lz4_nif:decompress_safe(CompressedBin, CompressedBinSize)),
    ?assertEqual(Bin, lz4_nif:decompress_safe(CompressedBin, CompressedBinSize + 1)),

    ?assertError(badarg, lz4_nif:decompress_safe(CompressedBin, CompressedBinSize - 1)),
    ?assertError(badarg, lz4_nif:decompress_safe(0, 0)),
    ?assertError(badarg, lz4_nif:decompress_safe(gen_bin(), byte_size(gen_bin()))).

%%%%%%%%%%%%%%
%% DocTests %%
%%%%%%%%%%%%%%

doc_test(_Config) ->
    shell_docs:test(lz4_nif, []).
%% doctest:module(lz4_nif).

%%%%%%%%%%%%%
%% Helpers %%
%%%%%%%%%%%%%

gen_bin() ->
    crypto:strong_rand_bytes(16).
