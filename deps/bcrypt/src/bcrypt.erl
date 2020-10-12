%% Copyright (c) 2011 Hunter Morris
%% Distributed under the MIT license; see LICENSE for details.
-module(bcrypt).
-author('Hunter Morris <hunter.morris@smarkets.com>').

%% API
-export([start/0, stop/0]).
-export([mechanism/0]).
-export([gen_salt/0, gen_salt/1, hashpw/2]).

-type mechanism() :: nif | port.
-type rounds() :: 4..31.
-type pwerr() :: invalid_salt | invalid_salt_length | invalid_rounds.

-export_type([ mechanism/0, rounds/0, pwerr/0 ]).

start() -> application:start(bcrypt).
stop()  -> application:stop(bcrypt).

-spec mechanism() -> mechanism().
mechanism() ->
    {ok, M} = application:get_env(bcrypt, mechanism),
    M.

-spec gen_salt() -> {ok, string()}.
gen_salt() ->
    do_gen_salt(mechanism()).

-spec gen_salt( rounds() ) -> {ok, string()}.
gen_salt(Rounds) ->
    do_gen_salt(mechanism(), Rounds).

-spec hashpw( string() | binary(), string() | binary() ) -> {ok, string()} | {error, pwerr()}.
hashpw(Password, Salt) ->
    do_hashpw(mechanism(), Password, Salt).

do_gen_salt(nif)  -> bcrypt_nif_worker:gen_salt();
do_gen_salt(port) -> bcrypt_pool:gen_salt().

do_gen_salt(nif, Rounds)  -> bcrypt_nif_worker:gen_salt(Rounds);
do_gen_salt(port, Rounds) -> bcrypt_pool:gen_salt(Rounds).

do_hashpw(nif, Password, Salt)  -> bcrypt_nif_worker:hashpw(Password, Salt);
do_hashpw(port, Password, Salt) -> bcrypt_pool:hashpw(Password, Salt).
