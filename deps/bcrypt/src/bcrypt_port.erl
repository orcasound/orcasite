%% Copyright (c) 2011 Hunter Morris
%% Distributed under the MIT license; see LICENSE for details.
-module(bcrypt_port).
-author('Hunter Morris <hunter.morris@smarkets.com>').

-behaviour(gen_server).

%% API
-export([start_link/0, stop/0]).
-export([gen_salt/1, gen_salt/2]).
-export([hashpw/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {
    port :: port(),
    default_log_rounds :: non_neg_integer(),
    cmd_from :: {pid(), term()}
    }).

-define(CMD_SALT, 0).
-define(CMD_HASH, 1).
-define(BCRYPT_ERROR(F, D), error_logger:error_msg(F, D)).
-define(BCRYPT_WARNING(F, D), error_logger:warning_msg(F, D)).

start_link() ->
    Dir = case code:priv_dir(bcrypt) of
              {error, bad_name} ->
                  case code:which(bcrypt) of
                      Filename when is_list(Filename) ->
                          filename:join(
                            [filename:dirname(Filename), "../priv"]);
                      _ ->
                          "../priv"
                  end;
              Priv -> Priv
          end,
    Port = filename:join(Dir, "bcrypt"),
    gen_server:start_link(?MODULE, [Port], []).

stop() -> gen_server:call(?MODULE, stop).

gen_salt(Pid) ->
    R = crypto:strong_rand_bytes(16),
    gen_server:call(Pid, {encode_salt, R}, infinity).

gen_salt(Pid, LogRounds) ->
    R = crypto:strong_rand_bytes(16),
    gen_server:call(Pid, {encode_salt, R, LogRounds}, infinity).

hashpw(Pid, Password, Salt) ->
    gen_server:call(Pid, {hashpw, Password, Salt}, infinity).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([Filename]) ->
    case file:read_file_info(Filename) of
        {ok, _Info} ->
            Port = open_port(
                     {spawn, Filename}, [{packet, 2}, binary, exit_status]),
            ok = bcrypt_pool:available(self()),
            {ok, Rounds} = application:get_env(bcrypt, default_log_rounds),
            {ok, #state{port = Port, default_log_rounds = Rounds}};
        {error, Reason} ->
            ?BCRYPT_ERROR("Can't open file ~p: ~p", [Filename, Reason]),
            {stop, error_opening_bcrypt_file}
    end.

terminate(_Reason, #state{port=Port}) ->
    catch port_close(Port),
    ok.

handle_call({encode_salt, R}, From, #state{default_log_rounds = LogRounds} = State) ->
    handle_call({encode_salt, R, LogRounds}, From, State);
handle_call({encode_salt, R, LogRounds}, From, #state{ cmd_from = undefined } = State) ->
    Port = State#state.port,
    Data = term_to_binary({?CMD_SALT, {iolist_to_binary(R), LogRounds}}),
    erlang:port_command(Port, Data),
    {noreply, State#state{ cmd_from = From }};
handle_call({encode_salt, _R, _Rounds}, From, #state{ cmd_from = CmdFrom } = State) ->
    ?BCRYPT_ERROR("bcrypt: Salt request from ~p whilst busy for ~p", [ From, CmdFrom ]),
    {reply, {error, {busy, From}}, State};
handle_call({hashpw, Password, Salt}, From, #state{ cmd_from = undefined } = State) ->
    Port = State#state.port,
    Data = term_to_binary({?CMD_HASH, {iolist_to_binary(Password), iolist_to_binary(Salt)}}),
    erlang:port_command(Port, Data),
    {noreply, State#state{ cmd_from = From }};
handle_call({hashpw, _Password, _Salt}, From, #state{ cmd_from = CmdFrom } = State) ->
    ?BCRYPT_ERROR("bcrypt: Hash request from ~p whilst busy for ~p", [ From, CmdFrom ]),
    {reply, {error, {busy, From}}, State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(Msg, _, State) ->
    {stop, {unknown_call, Msg}, State}.

handle_cast(Msg, State) ->
    {stop, {unknown_cast, Msg}, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info({Port, {data, Data}}, #state{ port = Port, cmd_from = From } = State) ->
    Reply =
        case binary_to_term(Data) of
            {_, Error} when is_atom(Error) ->
                {error, Error};
            {?CMD_SALT, Result} when is_binary(Result) ->
                {ok, binary_to_list(Result)};
            {?CMD_HASH, Result} when is_binary(Result) ->
                {ok, binary_to_list(Result)}
        end,
    gen_server:reply(From, Reply),
    ok = bcrypt_pool:available(self()),
    {noreply, State#state{ cmd_from = undefined }};
handle_info({Port, {exit_status, Status}}, #state{port=Port}=State) ->
    %% Rely on whomever is supervising this process to restart.
    ?BCRYPT_WARNING("Port died: ~p", [Status]),
    {stop, port_died, State};
handle_info(Msg, _) ->
    exit({unknown_info, Msg}).

