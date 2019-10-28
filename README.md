[![Build Status](https://travis-ci.org/tank-bohr/gen_server2.svg?branch=master)](https://travis-ci.org/tank-bohr/gen_server2)

gen_server2
=====

An OTP compatible gen_server

Build
-----

    $ rebar3 compile


Not implemented

- [ ] timeouts
- [ ] handle_continue -- just a sugar. but we can easily support it
- [ ] hibernation -- better to do every time.
- [ ] registered processes - Need to choose between gpoc/pg2/global/syn/whatever
- [ ] ~~enter_loop~~ there is no good reason to us it. There is no any popular open-source library that uses it. Requires `proc_lib`.
- [ ] ~~distibuted stuff~~ `multicall` and `abcast` shoud be done within registry

```
cloc src/gen_server2.erl
       1 text file.
       1 unique file.
       0 files ignored.

github.com/AlDanial/cloc v 1.80  T=0.01 s (78.9 files/s, 9312.6 lines/s)
-------------------------------------------------------------------------------
Language                     files          blank        comment           code
-------------------------------------------------------------------------------
Erlang                           1             18              0            100
-------------------------------------------------------------------------------
```
