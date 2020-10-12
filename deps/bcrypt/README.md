bcrypt
======

[![Build Status](https://travis-ci.org/erlangpack/bcrypt.svg?branch=master)](https://travis-ci.org/erlangpack/bcrypt)
[![Hex pm](http://img.shields.io/hexpm/v/bcrypt.svg?style=flat)](https://hex.pm/packages/bcrypt)

erlang-bcrypt is a wrapper around the OpenBSD Blowfish password hashing
algorithm, as described in 
[A Future-Adaptable Password Scheme](http://www.openbsd.org/papers/bcrypt-paper.ps) 
by Niels Provos and David Mazieres.

This bcrypt repository at erlangpack is in active maintainance and used
as the basis of the Hex package.


OTP Compatibility
-----------------

erlang-bcrypt is compatible with OTP 21.3 to 23.

Use version 1.0.3 on OTP versions before 21.3

In version 1.1.0 support for OTP 21.2 and earlier is removed
due to the removal of erl_interface in OTP 23.


Rebar.config
------------

erlang-bcrypt is on Hex:

  ```erlang
  {deps, [
      {bcrypt, "1.1.0"}
  ]}.
  ```

To use the master branch:

  ```erlang
  {deps, [
      {bcrypt, {git, ".*", {git, "https://github.com/erlangpack/bcrypt.git", {branch, "master"}}}
  ]}.
  ```


Basic build instructions
------------------------

1. Build it (project uses rebar3, a Makefile is included):

    ```shell
    make
    ```

2. Run it (simple way, starting sasl, crypto and bcrypt):

    ```shell
    $ ./rebar3 shell
    ===> Verifying dependencies...
    ===> Compiling bcrypt
    make: Nothing to be done for `all'.
    Erlang/OTP 23 [erts-11.0] [source] [64-bit] [smp:12:12] [ds:12:12:10] [async-threads:1] [hipe]

    Eshell V11.0  (abort with ^G)
    1> application:ensure_all_started(bcrypt).
    {ok,[bcrypt]}
    2>     
    ```

Basic usage instructions
------------------------

Hash a password using a salt with the default number of rounds:

```erlang
1> {ok, Salt} = bcrypt:gen_salt().
{ok,"$2a$12$sSS8Eg.ovVzaHzi1nUHYK."}
2> {ok, Hash} = bcrypt:hashpw("foo", Salt).
{ok,"$2a$12$sSS8Eg.ovVzaHzi1nUHYK.HbUIOdlQI0iS22Q5rd5z.JVVYH6sfm6"}
```

Verify the password:

```erlang
3> {ok, Hash} =:= bcrypt:hashpw("foo", Hash).
true
4> {ok, Hash} =:= bcrypt:hashpw("bar", Hash).
false
````

Configuration
-------------

The bcrypt application is configured by changing values in the
application's environment:

`default_log_rounds`
  Sets the default number of rounds which define the complexity of the
  hash function. Defaults to ``12``.

`mechanism`
  Specifies whether to use the NIF implementation (`'nif'`) or a
  pool of port programs (`'port'`). Defaults to `'nif'`.

  `Note: the NIF implementation no longer blocks the Erlang VM
  scheduler threads`

`pool_size`
  Specifies the size of the port program pool. Defaults to ``4``.

`nif_pool_size`
  Specifies the size of the nif program pool. Defaults to ``4``.

`nif_pool_max_overflow`
  Specifies the max workers to overflow of the nif program pool. Defaults to ``10``.

Run tests
---------

To run the eunit tests use:

```shell
make tests
```

Both the _port_ and the _NIF_ version of bcrypt are tested.
All tests should pass.

Original authors
----------------

Hunter Morris & [Mrinal Wadhwa](https://github.com/mrinalwadhwa).
