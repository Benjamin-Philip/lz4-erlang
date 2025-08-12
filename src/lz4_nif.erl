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
-nifs([compress_default/1, decompress_safe/2]).
-on_load(init/0).

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

init() ->
    erlang:load_nif("priv/lz4_nif", 0).

not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).
