-module(lz4_nif).
-moduledoc """
NIF interface for the LZ4 Block Format.

This module provides a direct mapping to the C API in `lz4.h`, eliminating extra
arguments when applicable. It is meant internal-use. Only use it if you need
fine-grained control. See the [LZ4
Manual](https://github.com/lz4/lz4/blob/cacca37747572717ceb1f156eb9840644205ca4f/doc/lz4_manual.html)
for more details.
""".

-export([compress_default/1, decompress_safe/2]).
-export([compress_bound/1, compress_fast/2, compress_dest_size/2, decompress_safe_partial/2]).
-nifs([
    %% Simple Functions
    compress_default/1,
    decompress_safe/2,
    %% Advanced Functions
    compress_bound/1,
    compress_fast/2,
    compress_dest_size/2,
    decompress_safe_partial/2
]).
-on_load(init/0).

%%%%%%%%%%%%%%%%%%%%%%
%% Simple Functions %%
%%%%%%%%%%%%%%%%%%%%%%

-doc """
Compresses a binary. Analogous to `LZ4_compress_default`.

Compresses a binary with the default configuration given a binary.
Raises a `badarg` otherwise:

```
1> lz4_nif:compress_default(<<1, 2, 3>>).
<<48, 1, 2, 3>>
2> lz4_nif:compress_default(foo).
** exception error: bad argument
     in function  lz4_nif:compress_default/1
        called as lz4_nif:compress_default(foo)
```
""".
-spec compress_default(Src :: binary()) -> Dst :: binary().
compress_default(_) ->
    not_loaded(?LINE).

-doc """
Decompresses a binary. Analogous to `LZ4_compress_safe`.

Safely decompresses a compressed binary given valid binary input (`Src`) and an
initial size for the uncompressed binary (`MaxDstSize`). Re-allocates the
returned uncompressed binary to its actual size:

```
1> Compressed = lz4_nif:compress_default(<<1, 2, 3>>).
<<48, 1, 2, 3>>
2> lz4_nif:decompress_safe(Compressed, 3).
<<1, 2, 3>>
3> lz4_nif:decompress_safe(Compressed, 10).
<<1, 2, 3>>
```

Raises a `badarg` if:

- `Src` binary is malformed or otherwise malicious
- `MaxDstSize` is too small (i.e. smaller than the actual decompressed binary)
- `Src` is not a binary
- `MaxDstSize` is not an integer

```
1> lz4_nif:decompress_safe(<<"malformed", 1, 2, 3>>, 10).
** exception error: bad argument
     in function  lz4_nif:decompress_safe/2
        called as lz4_nif:decompress_safe(<<109,97,108,102,111,114,109,101,100,1,2,3>>,10)
2> lz4_nif:decompress_safe(lz4_nif:compress_default(<<1, 2, 3>>), 2).
** exception error: bad argument
     in function  lz4_nif:decompress_safe/2
        called as lz4_nif:decompress_safe(<<48,1,2,3>>,2)
3> lz4_nif:decompress_safe(foo, 3).
** exception error: bad argument
     in function  lz4_nif:decompress_safe/2
        called as lz4_nif:decompress_safe(foo,3)
4> lz4_nif:decompress_safe(lz4_nif:compress_default(<<1, 2, 3>>), foo).
** exception error: bad argument
     in function  lz4_nif:decompress_safe/2
        called as lz4_nif:decompress_safe(<<48,1,2,3>>,foo)
```
""".
-spec decompress_safe(Src :: binary(), MaxDstSize :: pos_integer()) -> Dst :: binary().
decompress_safe(_, _) ->
    not_loaded(?LINE).

%%%%%%%%%%%%%%%%%%%%%%%%
%% Advanced Functions %%
%%%%%%%%%%%%%%%%%%%%%%%%

-doc """
Maximum compression output size in a worst case scenario. Analogous to
`LZ4_compressBound`.

Provides the maximum size that LZ4 compression may output in a worst case
scenario (input data not compressible). Raises badarg if input size is incorrect
(too large or negative):

```
1> lz4_nif:compress_bound(10).
26
2> lz4_nif:compress_bound(-1).
** exception error: bad argument
     in function  lz4_nif:compress_bound/1
        called as lz4_nif:compress_bound(-1)
3> lz4_nif:compress_bound(foo).
** exception error: bad argument
     in function  lz4_nif:compress_bound/1
        called as lz4_nif:compress_bound(foo)
```
""".
-spec compress_bound(InputSize :: pos_integer()) -> integer().
compress_bound(_) ->
    not_loaded(?LINE).

-doc """
Accelerated compression. Analogous to `LZ4_compress_fast`.

Compresses a binary at an accelerated speed, given a source binary and an
acceleration integer. See [LZ4
Manual](https://github.com/lz4/lz4/blob/cacca37747572717ceb1f156eb9840644205ca4f/doc/lz4_manual.html)
for more details regarding acceleration:

```
1> Bin = <<X || X <- lists:duplicate(9, <<1, 2, 3>>)>>.
<<1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3>>
2> lz4_nif:compress_fast(Bin, 6000).
<<240,12,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,
  1,2,3>>
3> lz4_nif:compress_fast(Bin, 2).
<<63,1,2,3,3,0,0,80,2,3,1,2,3>>
4> lz4_nif:compress_fast(Bin, -1).
<<63,1,2,3,3,0,0,80,2,3,1,2,3>>
```

Raises a `badarg` on non binary and integral values:

```
1> lz4_nif:compress_fast(foo, 1).
** exception error: bad argument
     in function  lz4_nif:compress_fast/2
        called as lz4_nif:compress_fast(foo,1)
2> lz4_nif:compress_fast(<<1, 2, 3>>, foo).
** exception error: bad argument
     in function  lz4_nif:compress_fast/2
        called as lz4_nif:compress_fast(<<1,2,3>>,foo)
```
""".
-spec compress_fast(Src :: binary(), Acceleration :: integer()) -> Dst :: binary().
compress_fast(_, _) ->
    not_loaded(?LINE).

-doc """
Compress as many bytes as possible into a binary of fixed length. Analogous to
`LZ4_compress_destSize`

Compresses as many bytes from binary `Src` as possible into a binary `Dst` of
fixed length `TargetDstSize`. Returns a tuple pair of the compressed binary and
the number of bytes compressed from `Src`:

```
1> {Compressed, BytesRead} = lz4_nif:compress_dest_size(<<1, 2, 3, 4>>, 3).
{<<32,1,2>>,2}
2> lz4_nif:decompress_safe(Compressed, BytesRead).
<<1,2>>
```

Raises a `badarg` on non binary and integral values:

```
1> lz4_nif:compress_dest_size(foo, 1).
** exception error: bad argument
     in function  lz4_nif:compress_dest_size/2
        called as lz4_nif:compress_dest_size(foo,1)
2> lz4_nif:compress_dest_size(<<1, 2, 3>>, foo).
** exception error: bad argument
     in function  lz4_nif:compress_dest_size/2
        called as lz4_nif:compress_dest_size(<<1,2,3>>,foo)
```
""".
-spec compress_dest_size(Src :: binary(), TargetDstSize :: integer()) ->
    {Dst :: binary(), BytesRead :: pos_integer()}.
compress_dest_size(_, _) ->
    not_loaded(?LINE).

-doc """
Decompresses as many bytes as possible into a binary of fixed length. Analogous
to `LZ4_decompress_safe_partial`.

Safely decompresses as many bytes as possible into a binary (`Dst`) of fixed
length (`TargetDstSize`), given a valid binary input (`Src`). Re-allocates the
returned uncompressed binary to its actual size:

```
1> Compressed = lz4_nif:compress_default(<<1, 2, 3>>).
<<48, 1, 2, 3>>
2> lz4_nif:decompress_safe_partial(Compressed, 2).
<<1, 2>>
3> lz4_nif:decompress_safe_partial(Compressed, 3).
<<1, 2, 3>>
4> lz4_nif:decompress_safe_partial(Compressed, 10).
<<1, 2, 3>>
```

Raises a `badarg` if:

- `Src` binary is malformed or otherwise malicious
- `Src` is not a binary
- `TargetDstSize` is not an integer

```
1> lz4_nif:decompress_safe_partial(<<"malformed", 1, 2, 3>>, 10).
** exception error: bad argument
     in function  lz4_nif:decompress_safe_partial/2
        called as lz4_nif:decompress_safe_partial(<<109,97,108,102,111,114,109,101,100,1,2,3>>,10)
2> lz4_nif:decompress_safe_partial(foo, 3).
** exception error: bad argument
     in function  lz4_nif:decompress_safe_partial/2
        called as lz4_nif:decompress_safe_partial(foo,3)
3> lz4_nif:decompress_safe_partial(lz4_nif:compress_default(<<1, 2, 3>>), foo).
** exception error: bad argument
     in function  lz4_nif:decompress_safe_partial/2
        called as lz4_nif:decompress_safe_partial(<<48,1,2,3>>,foo)
```

""".
-spec decompress_safe_partial(Src :: binary(), TargetDstSize :: pos_integer()) -> Dst :: binary().
decompress_safe_partial(_, _) ->
    not_loaded(?LINE).

%%%%%%%%%%%%%%%%%%%%
%% NIF Management %%
%%%%%%%%%%%%%%%%%%%%

init() ->
    erlang:load_nif("priv/lz4_nif", 0).

not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).
