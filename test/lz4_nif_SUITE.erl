-module(lz4_nif_SUITE).

-export([all/0]).
-export([compress_default/1, compress_hc/1, decompress_safe/1]).
-export([
    compress_bound/1,
    compress_fast/1,
    compress_dest_size/1,
    compress_hc_dest_size/1,
    decompress_safe_partial/1
]).
-export([doc_test/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

all() ->
    [
        %% Simple Functions
        compress_default,
        compress_hc,
        decompress_safe,

        %% Advanced Functions
        compress_bound,
        compress_fast,
        compress_dest_size,
        compress_hc_dest_size,
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

compress_hc(_Config) ->
    Bin = gen_bin(),
    CompressedBin = lz4_nif:compress_hc(Bin, 2),
    ?assertEqual(Bin, lz4_nif:decompress_safe(CompressedBin, byte_size(Bin))),

    LargeBin = gen_large_bin(),
    ?assertNotEqual(lz4_nif:compress_hc(LargeBin, 1), lz4_nif:compress_hc(LargeBin, 12)),

    ?assertError(badarg, lz4_nif:compress_hc(foo, 2)),
    ?assertError(badarg, lz4_nif:compress_hc(Bin, foo)),
    ?assertError(badarg, lz4_nif:compress_hc(Bin, -2)).

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
    ?assertEqual(lz4_nif:compress_dest_size(Bin, 100), lz4_nif:compress_dest_size(Bin, 200)),

    ?assertError(badarg, lz4_nif:compress_dest_size(Bin, -2)),
    ?assertError(badarg, lz4_nif:compress_dest_size(Bin, foo)),
    ?assertError(badarg, lz4_nif:compress_dest_size(foo, 3)).

compress_hc_dest_size(_Config) ->
    Bin = gen_bin(),
    {CompressBin, BytesRead} = lz4_nif:compress_hc_dest_size(Bin, 3, 9),
    ?assertEqual(binary:part(Bin, 0, BytesRead), lz4_nif:decompress_safe(CompressBin, BytesRead)),
    ?assertEqual(
        lz4_nif:compress_hc_dest_size(Bin, 100, 12), lz4_nif:compress_hc_dest_size(Bin, 200, 12)
    ),

    LargeBin = gen_large_bin(),
    ?assertNotEqual(
        lz4_nif:compress_hc_dest_size(LargeBin, 1024, 1),
        lz4_nif:compress_hc_dest_size(LargeBin, 1024, 12)
    ),

    ?assertError(badarg, lz4_nif:compress_hc_dest_size(Bin, -2, 12)),
    ?assertError(badarg, lz4_nif:compress_hc_dest_size(Bin, foo, 12)),
    ?assertError(badarg, lz4_nif:compress_hc_dest_size(Bin, 3, foo)),
    ?assertError(badarg, lz4_nif:compress_hc_dest_size(foo, 3, 12)).

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

gen_large_bin() ->
    Bin1 = gen_bin(),
    Bin2 = gen_bin(),
    Bin3 = gen_bin(),
    Bin4 = gen_bin(),
    Bin5 = gen_bin(),
    BinList = [rand:uniform(5) || _ <- lists:duplicate(30, 0)],
    <<
        case X of
            1 ->
                Bin1;
            2 ->
                Bin2;
            3 ->
                Bin3;
            4 ->
                Bin4;
            5 ->
                Bin5
        end
     || X <- BinList
    >>.

gen_bin() ->
    gen_bin(16).
gen_bin(N) ->
    crypto:strong_rand_bytes(N).
