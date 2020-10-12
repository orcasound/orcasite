-module(absinthe_parser).
-export([parse/1, parse_and_scan/1, format_error/1]).
-file("src/absinthe_parser.yrl", 262).

% Line-Level Utilities

extract_line({_Token, Line}) ->
  Line;
extract_line({_Token, Line, _Value}) ->
  Line;
extract_line(_) ->
  nil.

extract_child_line([Head|_]) ->
  extract_child_line(Head);
extract_child_line(#{loc := #{'start_line' := Line}}) ->
  Line;
extract_child_line(_) ->
  nil.


% Value-level Utilities

extract_atom({Value, _Line}) ->
  Value.

extract_binary(Value) when is_binary(Value) ->
  Value;

extract_binary({Token, _Line}) ->
  list_to_binary(atom_to_list(Token));

extract_binary({_Token, _Line, Value}) ->
  list_to_binary(Value).


% AST Generation

build_ast_node(Type, Node, #{'start_line' := nil}) ->
  build_ast_node(Type, Node, nil);
build_ast_node(Type, Node, Loc) ->
  'Elixir.Kernel':struct(list_to_atom("Elixir.Absinthe.Language." ++ atom_to_list(Type)), Node#{loc => Loc}).


% String

extract_quoted_string_token({_Token, _Line, Value}) ->
  iolist_to_binary(process_string(lists:sublist(Value, 2, length(Value) - 2))).

process_string(Escaped) ->
  process_string(Escaped, []).

process_string([], Acc) ->
  lists:reverse(Acc);
process_string([$\\, $" | T], Acc) ->
  process_string(T, [$" | Acc]);
process_string([$\\, $\\ | T], Acc) ->
  process_string(T, [$\\ | Acc]);
process_string([$\\, $/ | T], Acc) ->
  process_string(T, [$/ | Acc]);
process_string([$\\, $b | T], Acc) ->
  process_string(T, [$\b | Acc]);
process_string([$\\, $f | T], Acc) ->
  process_string(T, [$\f | Acc]);
process_string([$\\, $n | T], Acc) ->
  process_string(T, [$\n | Acc]);
process_string([$\\, $r | T], Acc) ->
  process_string(T, [$\r | Acc]);
process_string([$\\, $t | T], Acc) ->
  process_string(T, [$\t | Acc]);
process_string([$\\, $u, A, B, C, D | T], Acc) ->
  process_string(T, [hexlist_to_utf8_binary([A, B, C, D]) | Acc]);
process_string([H | T], Acc) ->
  process_string(T, [H | Acc]).

hexlist_to_utf8_binary(HexList) ->
  unicode:characters_to_binary([httpd_util:hexlist_to_integer(HexList)]).


% Block String

extract_quoted_block_string_token({_Token, _Line, Value}) ->
  iolist_to_binary(process_block_string(lists:sublist(Value, 4, length(Value) - 6))).

-spec process_block_string(string()) -> string().
process_block_string(Escaped) ->
  process_block_string(Escaped, []).

-spec process_block_string(string(), string()) -> string().
process_block_string([], Acc) ->
  block_string_value(lists:reverse(Acc));
process_block_string([$\r, $\n | T], Acc) -> process_block_string(T, [$\n | Acc]);
process_block_string([$\\, $", $", $" | T], Acc) -> process_block_string(T, [$", $", $"] ++ Acc);
process_block_string([H | T], Acc) -> process_block_string(T, [H | Acc]).

-spec block_string_value(string()) -> string().
block_string_value(Value) ->
  [FirstLine | Rest] = re:split(Value, "\n", [{return,list}]),
  Prefix = indentation_prefix(common_indent(Rest)),
  UnindentedLines = unindent(Rest, Prefix),
  Lines = trim_blank_lines([FirstLine | UnindentedLines]),
  string:join(Lines, "\n").

-spec trim_blank_lines([string()]) -> [string()].
trim_blank_lines(Lines) ->
  trim_blank_lines(trim_blank_lines(Lines, leading), trailing).

-spec trim_blank_lines([string()], leading | trailing) -> [string()].
trim_blank_lines(Lines, leading) ->
  lists:dropwhile(fun is_blank/1, Lines);
trim_blank_lines(Lines, trailing) ->
  lists:reverse(trim_blank_lines(lists:reverse(Lines), leading)).

-spec indentation_prefix(non_neg_integer()) -> string().
indentation_prefix(Indent) ->
  lists:map(fun(_) -> 32 end, lists:seq(1, Indent)).

-spec unindent([string()], string()) -> [string()].
unindent(Lines, Prefix) ->
  unindent(Lines, Prefix, []).

-spec unindent([string()], string(), [string()]) -> [string()].
unindent([], _Prefix, Result) ->
  lists:reverse(Result);
unindent([H | T], Prefix, Result) ->
  Processed = prefix(H, Prefix),
  unindent(T, Prefix, [Processed | Result]).

-spec prefix(string(), string()) -> string().
prefix(Line, []) ->
  Line;
prefix(Line, Prefix) ->
  Prefixed = lists:prefix(Prefix, Line),
  if
    Prefixed ->
      string:substr(Line, length(Prefix) + 1);
    true ->
      Line
  end.

-spec common_indent([string()]) -> non_neg_integer().
common_indent(Lines) ->
  case common_indent(Lines, noindent) of
    noindent ->
      0;
    Indent ->
      Indent
  end.

-spec common_indent([string()], noindent | non_neg_integer()) -> noindent | non_neg_integer().
common_indent([], Indent) ->
    Indent;
common_indent([H | T], Indent) ->
  CurrentIndent = leading_whitespace(H),
  if
    (CurrentIndent < length(H)) and ((Indent == noindent) or (CurrentIndent < Indent)) ->
      common_indent(T, CurrentIndent);
    true ->
      common_indent(T, Indent)
  end.

-spec leading_whitespace(string()) -> non_neg_integer().
leading_whitespace(BlockStringValue) ->
  leading_whitespace(BlockStringValue, 0).

-spec leading_whitespace(string(), non_neg_integer()) -> non_neg_integer().
leading_whitespace([], N) ->
  N;
leading_whitespace([32 | T], N) ->
  leading_whitespace(T, N + 1);
leading_whitespace([$\t | T], N) ->
  leading_whitespace(T, N + 1);
leading_whitespace([_H | _T], N) ->
  N.

-spec is_blank(string()) -> boolean().
is_blank(BlockStringValue) ->
    leading_whitespace(BlockStringValue) == length(BlockStringValue).


% Integer

extract_integer({_Token, _Line, Value}) ->
  {Int, []} = string:to_integer(Value), Int.


% Float

extract_float({_Token, _Line, Value}) ->
  {Float, []} = string:to_float(Value), Float.


% Boolean

extract_boolean({_Token, _Line, "true"}) ->
  true;
extract_boolean({_Token, _Line, "false"}) ->
  false.

-file("/usr/local/Cellar/erlang/23.0.2_1/lib/erlang/lib/parsetools-2.2/include/yeccpre.hrl", 0).
%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2018. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The parser generator will insert appropriate declarations before this line.%

-type yecc_ret() :: {'error', _} | {'ok', _}.

-spec parse(Tokens :: list()) -> yecc_ret().
parse(Tokens) ->
    yeccpars0(Tokens, {no_func, no_line}, 0, [], []).

-spec parse_and_scan({function() | {atom(), atom()}, [_]}
                     | {atom(), atom(), [_]}) -> yecc_ret().
parse_and_scan({F, A}) ->
    yeccpars0([], {{F, A}, no_line}, 0, [], []);
parse_and_scan({M, F, A}) ->
    Arity = length(A),
    yeccpars0([], {{fun M:F/Arity, A}, no_line}, 0, [], []).

-spec format_error(any()) -> [char() | list()].
format_error(Message) ->
    case io_lib:deep_char_list(Message) of
        true ->
            Message;
        _ ->
            io_lib:write(Message)
    end.

%% To be used in grammar files to throw an error message to the parser
%% toplevel. Doesn't have to be exported!
-compile({nowarn_unused_function, return_error/2}).
-spec return_error(integer(), any()) -> no_return().
return_error(Line, Message) ->
    throw({error, {Line, ?MODULE, Message}}).

-define(CODE_VERSION, "1.4").

yeccpars0(Tokens, Tzr, State, States, Vstack) ->
    try yeccpars1(Tokens, Tzr, State, States, Vstack)
    catch 
        error: Error: Stacktrace ->
            try yecc_error_type(Error, Stacktrace) of
                Desc ->
                    erlang:raise(error, {yecc_bug, ?CODE_VERSION, Desc},
                                 Stacktrace)
            catch _:_ -> erlang:raise(error, Error, Stacktrace)
            end;
        %% Probably thrown from return_error/2:
        throw: {error, {_Line, ?MODULE, _M}} = Error ->
            Error
    end.

yecc_error_type(function_clause, [{?MODULE,F,ArityOrArgs,_} | _]) ->
    case atom_to_list(F) of
        "yeccgoto_" ++ SymbolL ->
            {ok,[{atom,_,Symbol}],_} = erl_scan:string(SymbolL),
            State = case ArityOrArgs of
                        [S,_,_,_,_,_,_] -> S;
                        _ -> state_is_unknown
                    end,
            {Symbol, State, missing_in_goto_table}
    end.

yeccpars1([Token | Tokens], Tzr, State, States, Vstack) ->
    yeccpars2(State, element(1, Token), States, Vstack, Token, Tokens, Tzr);
yeccpars1([], {{F, A},_Line}, State, States, Vstack) ->
    case apply(F, A) of
        {ok, Tokens, Endline} ->
            yeccpars1(Tokens, {{F, A}, Endline}, State, States, Vstack);
        {eof, Endline} ->
            yeccpars1([], {no_func, Endline}, State, States, Vstack);
        {error, Descriptor, _Endline} ->
            {error, Descriptor}
    end;
yeccpars1([], {no_func, no_line}, State, States, Vstack) ->
    Line = 999999,
    yeccpars2(State, '$end', States, Vstack, yecc_end(Line), [],
              {no_func, Line});
yeccpars1([], {no_func, Endline}, State, States, Vstack) ->
    yeccpars2(State, '$end', States, Vstack, yecc_end(Endline), [],
              {no_func, Endline}).

%% yeccpars1/7 is called from generated code.
%%
%% When using the {includefile, Includefile} option, make sure that
%% yeccpars1/7 can be found by parsing the file without following
%% include directives. yecc will otherwise assume that an old
%% yeccpre.hrl is included (one which defines yeccpars1/5).
yeccpars1(State1, State, States, Vstack, Token0, [Token | Tokens], Tzr) ->
    yeccpars2(State, element(1, Token), [State1 | States],
              [Token0 | Vstack], Token, Tokens, Tzr);
yeccpars1(State1, State, States, Vstack, Token0, [], {{_F,_A}, _Line}=Tzr) ->
    yeccpars1([], Tzr, State, [State1 | States], [Token0 | Vstack]);
yeccpars1(State1, State, States, Vstack, Token0, [], {no_func, no_line}) ->
    Line = yecctoken_end_location(Token0),
    yeccpars2(State, '$end', [State1 | States], [Token0 | Vstack],
              yecc_end(Line), [], {no_func, Line});
yeccpars1(State1, State, States, Vstack, Token0, [], {no_func, Line}) ->
    yeccpars2(State, '$end', [State1 | States], [Token0 | Vstack],
              yecc_end(Line), [], {no_func, Line}).

%% For internal use only.
yecc_end({Line,_Column}) ->
    {'$end', Line};
yecc_end(Line) ->
    {'$end', Line}.

yecctoken_end_location(Token) ->
    try erl_anno:end_location(element(2, Token)) of
        undefined -> yecctoken_location(Token);
        Loc -> Loc
    catch _:_ -> yecctoken_location(Token)
    end.

-compile({nowarn_unused_function, yeccerror/1}).
yeccerror(Token) ->
    Text = yecctoken_to_string(Token),
    Location = yecctoken_location(Token),
    {error, {Location, ?MODULE, ["syntax error before: ", Text]}}.

-compile({nowarn_unused_function, yecctoken_to_string/1}).
yecctoken_to_string(Token) ->
    try erl_scan:text(Token) of
        undefined -> yecctoken2string(Token);
        Txt -> Txt
    catch _:_ -> yecctoken2string(Token)
    end.

yecctoken_location(Token) ->
    try erl_scan:location(Token)
    catch _:_ -> element(2, Token)
    end.

-compile({nowarn_unused_function, yecctoken2string/1}).
yecctoken2string({atom, _, A}) -> io_lib:write_atom(A);
yecctoken2string({integer,_,N}) -> io_lib:write(N);
yecctoken2string({float,_,F}) -> io_lib:write(F);
yecctoken2string({char,_,C}) -> io_lib:write_char(C);
yecctoken2string({var,_,V}) -> io_lib:format("~s", [V]);
yecctoken2string({string,_,S}) -> io_lib:write_string(S);
yecctoken2string({reserved_symbol, _, A}) -> io_lib:write(A);
yecctoken2string({_Cat, _, Val}) -> io_lib:format("~tp", [Val]);
yecctoken2string({dot, _}) -> "'.'";
yecctoken2string({'$end', _}) -> [];
yecctoken2string({Other, _}) when is_atom(Other) ->
    io_lib:write_atom(Other);
yecctoken2string(Other) ->
    io_lib:format("~tp", [Other]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



-file("src/absinthe_parser.erl", 372).

-dialyzer({nowarn_function, yeccpars2/7}).
yeccpars2(0=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(1=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_1(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(2=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(3=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_3(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(4=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(5=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_5(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(6=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_6(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(7=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(8=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_8(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(9=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_9(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(10=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_10(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(11=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_11(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(12=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_12(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(13=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_13(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(14=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_14(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(15=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_15(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(16=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_16(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(17=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_17(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(18=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(19=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(20=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(21=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_21(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(22=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(23=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(24=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_24(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(25=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(26=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(27=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(28=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(29=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(30=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(31=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(32=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_32(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(33=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_33(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(34=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_34(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(35=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_35(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(36=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_36(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(37=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_37(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(38=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_38(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(39=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(40=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(41=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(42=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(43=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(44=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(45=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(46=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(47=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(48=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(49=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(50=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(51=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(52=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(53=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(54=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(55=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(56=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_56(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(57=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_57(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(58=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_58(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(59=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_59(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(60=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_60(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(61=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_61(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(62=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(63=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(64=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_64(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(65=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_65(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(66=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_66(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(67=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_67(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(68=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_60(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(69=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_69(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(70=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_70(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(71=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_71(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(72=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_72(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(73=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_73(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(74=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_74(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(75=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_75(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(76=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_76(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(77=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(78=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(79=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_79(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(80=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_80(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(81=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_81(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(82=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_82(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(83=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_83(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(84=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_84(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(85=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_85(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(86=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_86(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(87=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(88=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_88(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(89=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_89(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(90=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_90(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(91=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(92=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(93=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_93(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(94=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_94(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(95=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_95(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(96=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_96(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(97=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_97(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(98=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(99=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_99(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(100=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_100(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(101=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_101(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(102=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_102(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(103=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_103(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(104=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_104(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(105=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_105(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(106=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_106(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(107=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_107(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(108=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(109=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_109(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(110=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(111=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(112=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_112(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(113=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_113(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(114=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_114(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(115=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_115(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(116=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_116(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(117=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_117(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(118=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_118(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(119=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_119(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(120=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_120(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(121=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_121(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(122=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_122(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(123=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_123(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(124=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_124(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(125=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_125(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(126=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_126(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(127=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_127(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(128=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_128(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(129=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_129(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(130=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_130(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(131=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_131(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(132=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_132(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(133=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_133(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(134=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_134(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(135=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_135(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(136=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_136(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(137=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(138=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_138(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(139=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_139(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(140=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(141=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_141(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(142=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(143=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_143(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(144=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_144(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(145=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_145(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(146=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_146(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(147=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(148=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(149=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_149(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(150=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_150(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(151=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_151(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(152=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_152(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(153=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(154=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_154(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(155=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_155(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(156=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(157=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(158=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_158(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(159=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_159(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(160=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_160(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(161=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_161(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(162=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(163=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_163(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(164=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_164(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(165=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_165(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(166=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_166(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(167=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_167(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(168=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_168(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(169=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_169(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(170=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_170(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(171=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_171(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(172=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_172(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(173=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(174=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_174(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(175=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_175(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(176=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_176(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(177=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(178=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_178(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(179=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_179(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(180=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(181=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_181(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(182=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_182(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(183=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(184=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_184(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(185=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_185(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(186=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_186(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(187=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_187(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(188=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(189=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_189(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(190=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_190(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(191=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_191(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(192=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(193=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_193(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(194=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_194(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(195=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(196=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_196(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(197=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_197(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(198=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_198(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(199=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(200=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_200(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(201=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_201(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(202=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(203=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_203(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(204=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_204(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(205=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_205(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(206=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_206(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(207=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_207(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(208=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_208(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(209=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(210=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_210(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(211=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_211(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(212=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(213=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_213(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(214=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_214(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(215=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_215(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(216=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_216(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(217=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(218=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_218(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(219=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_219(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(220=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(221=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_221(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(222=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_222(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(223=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_223(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(224=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(225=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_64(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(226=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_226(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(227=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_60(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(228=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_228(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(229=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_229(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(230=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_230(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(231=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_231(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(232=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(233=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_233(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(234=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_234(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(235=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_235(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(236=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_236(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(237=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_237(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(238=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_238(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(239=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(240=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_240(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(241=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_241(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(242=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(243=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_243(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(244=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_244(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(245=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(246=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_246(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(247=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_247(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(248=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_248(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(249=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(250=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_250(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(251=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(252=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_252(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(253=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_253(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(254=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_254(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(255=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_64(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(256=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_256(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(257=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_257(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(258=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_258(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(259=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_259(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(260=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_260(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(261=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_261(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(262=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(263=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_263(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(264=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_264(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(265=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_265(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(266=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_266(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(267=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_64(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(268=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_268(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(269=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_60(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(270=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_270(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(271=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_271(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(272=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_60(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(273=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_273(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(274=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_274(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(275=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_60(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(276=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_276(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(Other, _, _, _, _, _, _) ->
 erlang:error({yecc_bug,"1.4",{missing_state_in_action_table, Other}}).

-dialyzer({nowarn_function, yeccpars2_0/7}).
yeccpars2_0(S, directive, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, enum, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, extend, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, fragment, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, input, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, interface, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, mutation, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, query, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, scalar, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, schema, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, subscription, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, union, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'TypeDefinition\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'TypeDefinition\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Definition\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_4_(Stack),
 'yeccgoto_\'OperationDefinition\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'TypeDefinition\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'TypeDefinition\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_7(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 258, Ss, Stack, T, Ts, Tzr);
yeccpars2_7(S, on, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_7(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_21(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Definition\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'TypeDefinition\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'TypeDefinition\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'TypeDefinition\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Definition\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'TypeDefinition\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_14/7}).
yeccpars2_14(_S, '$end', _Ss, Stack, _T, _Ts, _Tzr) ->
 {ok, hd(Stack)};
yeccpars2_14(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'TypeDefinition\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_16_(Stack),
 'yeccgoto_\'Document\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_17(S, directive, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_17(S, enum, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_17(S, extend, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_17(S, fragment, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_17(S, input, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_17(S, interface, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_17(S, mutation, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_17(S, query, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_17(S, scalar, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_17(S, schema, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_17(S, subscription, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_17(S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_17(S, union, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_17(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_17_(Stack),
 'yeccgoto_\'Definitions\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_18/7}).
yeccpars2_18(S, '@', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 242, Ss, Stack, T, Ts, Tzr);
yeccpars2_18(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_19(S, on, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_21(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_20/7}).
yeccpars2_20(S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_20(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_21/7}).
yeccpars2_21(S, directive, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_21(S, enum, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_21(S, extend, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_21(S, fragment, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_21(S, implements, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_21(S, input, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_21(S, interface, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_21(S, mutation, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_21(S, name, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_21(S, query, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_21(S, scalar, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_21(S, schema, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_21(S, subscription, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_21(S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_21(S, union, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_21(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_22: see yeccpars2_19

%% yeccpars2_23: see yeccpars2_19

yeccpars2_24(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_24_(Stack),
 'yeccgoto_\'OperationType\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_25_(Stack),
 'yeccgoto_\'OperationType\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_26: see yeccpars2_19

-dialyzer({nowarn_function, yeccpars2_27/7}).
yeccpars2_27(S, '@', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_27(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 199, Ss, Stack, T, Ts, Tzr);
yeccpars2_27(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_28_(Stack),
 'yeccgoto_\'OperationType\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_29: see yeccpars2_19

%% yeccpars2_30: see yeccpars2_19

yeccpars2_31(S, '...', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_31(S, on, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_31(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_21(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_32/7}).
yeccpars2_32(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 134, Ss, Stack, T, Ts, Tzr);
yeccpars2_32(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_33(S, '...', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(S, directive, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(S, enum, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(S, extend, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(S, fragment, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(S, implements, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(S, input, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(S, interface, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(S, mutation, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(S, name, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(S, on, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(S, query, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(S, scalar, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(S, schema, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(S, subscription, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(S, union, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_33_(Stack),
 'yeccgoto_\'Selections\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Name\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_35(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_35(S, ':', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 128, Ss, Stack, T, Ts, Tzr);
yeccpars2_35(S, '@', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_35(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_35_(Stack),
 'yeccgoto_\'Field\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Selection\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Selection\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_38(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Selection\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_39: see yeccpars2_19

yeccpars2_40(S, '@', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_40(S, on, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_40(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_40(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_21(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_41_(Stack),
 'yeccgoto_\'NameWithoutOn\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_42_(Stack),
 'yeccgoto_\'NameWithoutOn\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_43_(Stack),
 'yeccgoto_\'NameWithoutOn\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_44_(Stack),
 'yeccgoto_\'NameWithoutOn\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_45(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_45_(Stack),
 'yeccgoto_\'NameWithoutOn\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_46(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_46_(Stack),
 'yeccgoto_\'NameWithoutOn\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_47_(Stack),
 'yeccgoto_\'NameWithoutOn\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_48_(Stack),
 'yeccgoto_\'NameWithoutOn\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'NameWithoutOn\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_50_(Stack),
 'yeccgoto_\'Name\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_51(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_51_(Stack),
 'yeccgoto_\'NameWithoutOn\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_52_(Stack),
 'yeccgoto_\'NameWithoutOn\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_53_(Stack),
 'yeccgoto_\'NameWithoutOn\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_54_(Stack),
 'yeccgoto_\'NameWithoutOn\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_55_(Stack),
 'yeccgoto_\'NameWithoutOn\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_56(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_56_(Stack),
 'yeccgoto_\'NameWithoutOn\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_57(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_57_(Stack),
 'yeccgoto_\'InlineFragment\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_58(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_58_(Stack),
 'yeccgoto_\'FragmentName\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_59(S, '@', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_59(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_59_(Stack),
 'yeccgoto_\'FragmentSpread\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_60/7}).
yeccpars2_60(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_60(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_61(S, '@', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_61_(Stack),
 'yeccgoto_\'Directives\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_62(S, on, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_62(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_21(S, Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_63: see yeccpars2_19

-dialyzer({nowarn_function, yeccpars2_64/7}).
yeccpars2_64(S, '@', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_65(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'TypeCondition\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_66(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_66_(Stack),
 'yeccgoto_\'NamedType\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_67_(Stack),
 'yeccgoto_\'InlineFragment\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_68: see yeccpars2_60

yeccpars2_69(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_69_(Stack),
 'yeccgoto_\'InlineFragment\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_70(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_70(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_70_(Stack),
 'yeccgoto_\'Directive\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_71(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_71(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_71_(Stack),
 'yeccgoto_\'Directive\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_72(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_72_(Stack),
 'yeccgoto_\'Directive\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_73(S, on, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_73(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_21(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_74/7}).
yeccpars2_74(S, ':', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 111, Ss, Stack, T, Ts, Tzr);
yeccpars2_74(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_75/7}).
yeccpars2_75(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 110, Ss, Stack, T, Ts, Tzr);
yeccpars2_75(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_76(S, directive, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_76(S, enum, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_76(S, extend, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_76(S, fragment, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_76(S, implements, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_76(S, input, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_76(S, interface, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_76(S, mutation, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_76(S, name, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_76(S, on, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_76(S, query, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_76(S, scalar, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_76(S, schema, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_76(S, subscription, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_76(S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_76(S, union, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_76(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_76_(Stack),
 'yeccgoto_\'ArgumentList\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_77/7}).
yeccpars2_77(S, ':', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_77(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_78(S, '$', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 85, Ss, Stack, T, Ts, Tzr);
yeccpars2_78(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 86, Ss, Stack, T, Ts, Tzr);
yeccpars2_78(S, block_string_value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 87, Ss, Stack, T, Ts, Tzr);
yeccpars2_78(S, boolean_value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 88, Ss, Stack, T, Ts, Tzr);
yeccpars2_78(S, float_value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 89, Ss, Stack, T, Ts, Tzr);
yeccpars2_78(S, int_value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 90, Ss, Stack, T, Ts, Tzr);
yeccpars2_78(S, null, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 91, Ss, Stack, T, Ts, Tzr);
yeccpars2_78(S, on, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_78(S, string_value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 92, Ss, Stack, T, Ts, Tzr);
yeccpars2_78(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 93, Ss, Stack, T, Ts, Tzr);
yeccpars2_78(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_21(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_79(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Value\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_80(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_80_(Stack),
 'yeccgoto_\'Argument\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_81(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_81_(Stack),
 'yeccgoto_\'Value\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_82_(Stack),
 'yeccgoto_\'EnumValue\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_83(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_83_(Stack),
 'yeccgoto_\'Value\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_84(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_84_(Stack),
 'yeccgoto_\'Value\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_85(S, on, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 108, Ss, Stack, T, Ts, Tzr);
yeccpars2_85(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_21(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_86(S, '$', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 85, Ss, Stack, T, Ts, Tzr);
yeccpars2_86(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 86, Ss, Stack, T, Ts, Tzr);
yeccpars2_86(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 104, Ss, Stack, T, Ts, Tzr);
yeccpars2_86(S, block_string_value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 87, Ss, Stack, T, Ts, Tzr);
yeccpars2_86(S, boolean_value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 88, Ss, Stack, T, Ts, Tzr);
yeccpars2_86(S, float_value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 89, Ss, Stack, T, Ts, Tzr);
yeccpars2_86(S, int_value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 90, Ss, Stack, T, Ts, Tzr);
yeccpars2_86(S, null, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 91, Ss, Stack, T, Ts, Tzr);
yeccpars2_86(S, on, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_86(S, string_value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 92, Ss, Stack, T, Ts, Tzr);
yeccpars2_86(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 93, Ss, Stack, T, Ts, Tzr);
yeccpars2_86(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_21(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_87_(Stack),
 'yeccgoto_\'Value\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_88(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_88_(Stack),
 'yeccgoto_\'Value\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_89(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_89_(Stack),
 'yeccgoto_\'Value\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_90(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_90_(Stack),
 'yeccgoto_\'Value\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_91(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_91_(Stack),
 'yeccgoto_\'Value\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_92(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_92_(Stack),
 'yeccgoto_\'Value\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_93(S, on, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 97, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_21(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_94/7}).
yeccpars2_94(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 101, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_95(S, directive, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_95(S, enum, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_95(S, extend, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_95(S, fragment, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_95(S, implements, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_95(S, input, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_95(S, interface, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_95(S, mutation, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_95(S, name, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_95(S, on, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_95(S, query, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_95(S, scalar, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_95(S, schema, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_95(S, subscription, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_95(S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_95(S, union, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_95(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_95_(Stack),
 'yeccgoto_\'ObjectFields\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_96/7}).
yeccpars2_96(S, ':', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 98, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_97(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_97_(Stack),
 'yeccgoto_\'ObjectValue\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_98: see yeccpars2_78

yeccpars2_99(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_99_(Stack),
 'yeccgoto_\'ObjectField\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_100(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_100_(Stack),
 'yeccgoto_\'ObjectFields\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_101(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_101_(Stack),
 'yeccgoto_\'ObjectValue\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_102/7}).
yeccpars2_102(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 106, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_103(S, '$', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 85, Ss, Stack, T, Ts, Tzr);
yeccpars2_103(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 86, Ss, Stack, T, Ts, Tzr);
yeccpars2_103(S, block_string_value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 87, Ss, Stack, T, Ts, Tzr);
yeccpars2_103(S, boolean_value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 88, Ss, Stack, T, Ts, Tzr);
yeccpars2_103(S, directive, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_103(S, enum, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_103(S, extend, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_103(S, float_value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 89, Ss, Stack, T, Ts, Tzr);
yeccpars2_103(S, fragment, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_103(S, implements, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_103(S, input, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_103(S, int_value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 90, Ss, Stack, T, Ts, Tzr);
yeccpars2_103(S, interface, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_103(S, mutation, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_103(S, name, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_103(S, null, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 91, Ss, Stack, T, Ts, Tzr);
yeccpars2_103(S, on, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_103(S, query, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_103(S, scalar, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_103(S, schema, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_103(S, string_value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 92, Ss, Stack, T, Ts, Tzr);
yeccpars2_103(S, subscription, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_103(S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_103(S, union, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_103(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 93, Ss, Stack, T, Ts, Tzr);
yeccpars2_103(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_(Stack),
 'yeccgoto_\'Values\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_104(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_104_(Stack),
 'yeccgoto_\'ListValue\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_105(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_105_(Stack),
 'yeccgoto_\'Values\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_106(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_106_(Stack),
 'yeccgoto_\'ListValue\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_107(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_107_(Stack),
 'yeccgoto_\'Variable\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_108(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_108_(Stack),
 'yeccgoto_\'Variable\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_109_(Stack),
 'yeccgoto_\'ArgumentList\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_110(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_110_(Stack),
 'yeccgoto_\'Arguments\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_111: see yeccpars2_78

yeccpars2_112(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_112_(Stack),
 'yeccgoto_\'Argument\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_113(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_113_(Stack),
 'yeccgoto_\'Directive\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_114(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_114_(Stack),
 'yeccgoto_\'Directives\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_115(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_115_(Stack),
 'yeccgoto_\'InlineFragment\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_116(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_116_(Stack),
 'yeccgoto_\'FragmentSpread\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_117(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_117(S, '@', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_117(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_117(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_117_(Stack),
 'yeccgoto_\'Field\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_118(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_118_(Stack),
 'yeccgoto_\'Field\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_119(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_119(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_119_(Stack),
 'yeccgoto_\'Field\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_120(S, '@', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_120(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_120_(Stack),
 'yeccgoto_\'Field\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_121(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_121_(Stack),
 'yeccgoto_\'Field\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_122(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_122_(Stack),
 'yeccgoto_\'Field\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_123(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_123_(Stack),
 'yeccgoto_\'Field\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_124(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_124_(Stack),
 'yeccgoto_\'Field\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_125(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_125_(Stack),
 'yeccgoto_\'Field\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_126(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_126(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_126_(Stack),
 'yeccgoto_\'Field\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_127(S, '@', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_127(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_127(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_127_(Stack),
 'yeccgoto_\'Field\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_128(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_128_(Stack),
 'yeccgoto_\'Alias\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_129(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_129_(Stack),
 'yeccgoto_\'Field\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_130(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_130_(Stack),
 'yeccgoto_\'Field\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_131(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_131_(Stack),
 'yeccgoto_\'Field\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_132(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_132_(Stack),
 'yeccgoto_\'Field\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_133(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_133_(Stack),
 'yeccgoto_\'Selections\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_134(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_134_(Stack),
 'yeccgoto_\'SelectionSet\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_135/7}).
yeccpars2_135(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 137, Ss, Stack, T, Ts, Tzr);
yeccpars2_135(S, '@', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_135(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_136/7}).
yeccpars2_136(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 142, Ss, Stack, T, Ts, Tzr);
yeccpars2_136(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_137: see yeccpars2_19

yeccpars2_138(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_138_(Stack),
 'yeccgoto_\'UnionTypeDefinition\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_139(S, '|', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 140, Ss, Stack, T, Ts, Tzr);
yeccpars2_139(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_139_(Stack),
 'yeccgoto_\'UnionMembers\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_140: see yeccpars2_19

yeccpars2_141(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_141_(Stack),
 'yeccgoto_\'UnionMembers\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_142: see yeccpars2_19

yeccpars2_143(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_143_(Stack),
 'yeccgoto_\'UnionTypeDefinition\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_144/7}).
yeccpars2_144(S, '@', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_144(S, implements, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 147, Ss, Stack, T, Ts, Tzr);
yeccpars2_144(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 148, Ss, Stack, T, Ts, Tzr);
yeccpars2_144(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_145/7}).
yeccpars2_145(S, '@', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_145(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 192, Ss, Stack, T, Ts, Tzr);
yeccpars2_145(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_146/7}).
yeccpars2_146(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_146(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_147: see yeccpars2_19

%% yeccpars2_148: see yeccpars2_19

-dialyzer({nowarn_function, yeccpars2_149/7}).
yeccpars2_149(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 156, Ss, Stack, T, Ts, Tzr);
yeccpars2_149(S, ':', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 157, Ss, Stack, T, Ts, Tzr);
yeccpars2_149(S, '@', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_149(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_150/7}).
yeccpars2_150(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 153, Ss, Stack, T, Ts, Tzr);
yeccpars2_150(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_151(S, directive, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(S, enum, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(S, extend, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(S, fragment, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(S, implements, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(S, input, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(S, interface, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(S, mutation, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(S, name, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(S, on, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(S, query, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(S, scalar, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(S, schema, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(S, subscription, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(S, union, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_151_(Stack),
 'yeccgoto_\'FieldDefinitionList\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_152_(Stack),
 'yeccgoto_\'FieldDefinitionList\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_153(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_153_(Stack),
 'yeccgoto_\'ObjectTypeDefinition\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_154/7}).
yeccpars2_154(S, ':', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_154(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_155/7}).
yeccpars2_155(S, ':', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 180, Ss, Stack, T, Ts, Tzr);
yeccpars2_155(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_156: see yeccpars2_19

yeccpars2_157(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 162, Ss, Stack, T, Ts, Tzr);
yeccpars2_157(S, on, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_157(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_21(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_158(S, '@', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_158(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_158_(Stack),
 'yeccgoto_\'FieldDefinition\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_159(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Type\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_160(S, '!', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 166, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Type\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_161(S, '!', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 165, Ss, Stack, T, Ts, Tzr);
yeccpars2_161(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Type\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_162: see yeccpars2_157

-dialyzer({nowarn_function, yeccpars2_163/7}).
yeccpars2_163(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 164, Ss, Stack, T, Ts, Tzr);
yeccpars2_163(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_164(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_164_(Stack),
 'yeccgoto_\'ListType\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_165(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_165_(Stack),
 'yeccgoto_\'NonNullType\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_166(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_166_(Stack),
 'yeccgoto_\'NonNullType\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_167(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_167_(Stack),
 'yeccgoto_\'FieldDefinition\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_168/7}).
yeccpars2_168(S, ':', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 173, Ss, Stack, T, Ts, Tzr);
yeccpars2_168(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_169/7}).
yeccpars2_169(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 172, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_170(S, directive, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_170(S, enum, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_170(S, extend, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_170(S, fragment, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_170(S, implements, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_170(S, input, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_170(S, interface, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_170(S, mutation, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_170(S, name, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_170(S, on, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_170(S, query, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_170(S, scalar, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_170(S, schema, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_170(S, subscription, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_170(S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_170(S, union, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_170(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_170_(Stack),
 'yeccgoto_\'InputValueDefinitionList\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_171(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_171_(Stack),
 'yeccgoto_\'InputValueDefinitionList\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_172(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_172_(Stack),
 'yeccgoto_\'ArgumentsDefinition\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_173: see yeccpars2_157

yeccpars2_174(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 177, Ss, Stack, T, Ts, Tzr);
yeccpars2_174(S, '@', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_174(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_174_(Stack),
 'yeccgoto_\'InputValueDefinition\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_175(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_175_(Stack),
 'yeccgoto_\'InputValueDefinition\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_176(S, '@', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_176(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_176_(Stack),
 'yeccgoto_\'InputValueDefinition\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_177: see yeccpars2_78

yeccpars2_178(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_178_(Stack),
 'yeccgoto_\'DefaultValue\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_179(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_179_(Stack),
 'yeccgoto_\'InputValueDefinition\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_180: see yeccpars2_157

yeccpars2_181(S, '@', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_181_(Stack),
 'yeccgoto_\'FieldDefinition\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_182(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_182_(Stack),
 'yeccgoto_\'FieldDefinition\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_183: see yeccpars2_157

yeccpars2_184(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_184_(Stack),
 'yeccgoto_\'FieldDefinition\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_185(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_185_(Stack),
 'yeccgoto_\'ImplementsInterfaces\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_186(S, directive, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_186(S, enum, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_186(S, extend, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_186(S, fragment, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_186(S, implements, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_186(S, input, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_186(S, interface, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_186(S, mutation, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_186(S, name, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_186(S, on, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_186(S, query, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_186(S, scalar, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_186(S, schema, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_186(S, subscription, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_186(S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_186(S, union, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_186(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_186_(Stack),
 'yeccgoto_\'NamedTypeList\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_187(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_187_(Stack),
 'yeccgoto_\'NamedTypeList\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_188: see yeccpars2_19

-dialyzer({nowarn_function, yeccpars2_189/7}).
yeccpars2_189(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_189(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_190(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_190_(Stack),
 'yeccgoto_\'ObjectTypeDefinition\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_191/7}).
yeccpars2_191(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 195, Ss, Stack, T, Ts, Tzr);
yeccpars2_191(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_192: see yeccpars2_19

-dialyzer({nowarn_function, yeccpars2_193/7}).
yeccpars2_193(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 194, Ss, Stack, T, Ts, Tzr);
yeccpars2_193(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_194(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_194_(Stack),
 'yeccgoto_\'ObjectTypeDefinition\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_195: see yeccpars2_19

-dialyzer({nowarn_function, yeccpars2_196/7}).
yeccpars2_196(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 197, Ss, Stack, T, Ts, Tzr);
yeccpars2_196(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_197(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_197_(Stack),
 'yeccgoto_\'ObjectTypeDefinition\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_198/7}).
yeccpars2_198(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 202, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_199: see yeccpars2_19

-dialyzer({nowarn_function, yeccpars2_200/7}).
yeccpars2_200(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 201, Ss, Stack, T, Ts, Tzr);
yeccpars2_200(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_201(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_201_(Stack),
 'yeccgoto_\'SchemaDefinition\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_202: see yeccpars2_19

-dialyzer({nowarn_function, yeccpars2_203/7}).
yeccpars2_203(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 204, Ss, Stack, T, Ts, Tzr);
yeccpars2_203(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_204(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_204_(Stack),
 'yeccgoto_\'SchemaDefinition\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_205(S, '@', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_205(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_205_(Stack),
 'yeccgoto_\'ScalarTypeDefinition\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_206(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_206_(Stack),
 'yeccgoto_\'ScalarTypeDefinition\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_207/7}).
yeccpars2_207(S, '@', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_207(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 209, Ss, Stack, T, Ts, Tzr);
yeccpars2_207(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_208/7}).
yeccpars2_208(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 212, Ss, Stack, T, Ts, Tzr);
yeccpars2_208(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_209: see yeccpars2_19

-dialyzer({nowarn_function, yeccpars2_210/7}).
yeccpars2_210(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 211, Ss, Stack, T, Ts, Tzr);
yeccpars2_210(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_211(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_211_(Stack),
 'yeccgoto_\'InterfaceTypeDefinition\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_212: see yeccpars2_19

-dialyzer({nowarn_function, yeccpars2_213/7}).
yeccpars2_213(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 214, Ss, Stack, T, Ts, Tzr);
yeccpars2_213(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_214(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_214_(Stack),
 'yeccgoto_\'InterfaceTypeDefinition\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_215/7}).
yeccpars2_215(S, '@', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_215(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 217, Ss, Stack, T, Ts, Tzr);
yeccpars2_215(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_216/7}).
yeccpars2_216(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 220, Ss, Stack, T, Ts, Tzr);
yeccpars2_216(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_217: see yeccpars2_19

-dialyzer({nowarn_function, yeccpars2_218/7}).
yeccpars2_218(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 219, Ss, Stack, T, Ts, Tzr);
yeccpars2_218(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_219(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_219_(Stack),
 'yeccgoto_\'InputObjectTypeDefinition\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_220: see yeccpars2_19

-dialyzer({nowarn_function, yeccpars2_221/7}).
yeccpars2_221(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 222, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_222(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_222_(Stack),
 'yeccgoto_\'InputObjectTypeDefinition\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_223/7}).
yeccpars2_223(S, on, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 224, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_224: see yeccpars2_19

%% yeccpars2_225: see yeccpars2_64

yeccpars2_226(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_226_(Stack),
 'yeccgoto_\'Fragment\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_227: see yeccpars2_60

yeccpars2_228(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_228_(Stack),
 'yeccgoto_\'Fragment\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_229(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_229_(Stack),
 'yeccgoto_\'TypeExtensionDefinition\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_230/7}).
yeccpars2_230(S, '@', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_230(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 232, Ss, Stack, T, Ts, Tzr);
yeccpars2_230(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_231/7}).
yeccpars2_231(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 239, Ss, Stack, T, Ts, Tzr);
yeccpars2_231(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_232: see yeccpars2_19

-dialyzer({nowarn_function, yeccpars2_233/7}).
yeccpars2_233(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 238, Ss, Stack, T, Ts, Tzr);
yeccpars2_233(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_234(S, directive, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_234(S, enum, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_234(S, extend, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_234(S, fragment, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_234(S, implements, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_234(S, input, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_234(S, interface, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_234(S, mutation, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_234(S, name, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_234(S, on, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_234(S, query, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_234(S, scalar, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_234(S, schema, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_234(S, subscription, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_234(S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_234(S, union, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_234(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_234_(Stack),
 'yeccgoto_\'EnumValueDefinitionList\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_235(S, '@', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_235(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_235_(Stack),
 'yeccgoto_\'EnumValueDefinition\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_236(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_236_(Stack),
 'yeccgoto_\'EnumValueDefinition\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_237(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_237_(Stack),
 'yeccgoto_\'EnumValueDefinitionList\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_238(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_238_(Stack),
 'yeccgoto_\'EnumTypeDefinition\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_239: see yeccpars2_19

-dialyzer({nowarn_function, yeccpars2_240/7}).
yeccpars2_240(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 241, Ss, Stack, T, Ts, Tzr);
yeccpars2_240(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_241(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_241_(Stack),
 'yeccgoto_\'EnumTypeDefinition\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_242: see yeccpars2_19

-dialyzer({nowarn_function, yeccpars2_243/7}).
yeccpars2_243(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 156, Ss, Stack, T, Ts, Tzr);
yeccpars2_243(S, on, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 245, Ss, Stack, T, Ts, Tzr);
yeccpars2_243(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_244/7}).
yeccpars2_244(S, on, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 251, Ss, Stack, T, Ts, Tzr);
yeccpars2_244(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_245: see yeccpars2_19

yeccpars2_246(S, '|', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 249, Ss, Stack, T, Ts, Tzr);
yeccpars2_246(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_246_(Stack),
 'yeccgoto_\'DirectiveDefinitionLocations\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_247(S, '@', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_247(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_247_(Stack),
 'yeccgoto_\'DirectiveDefinition\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_248(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_248_(Stack),
 'yeccgoto_\'DirectiveDefinition\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_249: see yeccpars2_19

yeccpars2_250(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_250_(Stack),
 'yeccgoto_\'DirectiveDefinitionLocations\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_251: see yeccpars2_19

yeccpars2_252(S, '@', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_252(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_252_(Stack),
 'yeccgoto_\'DirectiveDefinition\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_253(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_253_(Stack),
 'yeccgoto_\'DirectiveDefinition\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_254(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_254_(Stack),
 'yeccgoto_\'Definitions\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_255: see yeccpars2_64

yeccpars2_256(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_256_(Stack),
 'yeccgoto_\'OperationDefinition\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_257/7}).
yeccpars2_257(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 258, Ss, Stack, T, Ts, Tzr);
yeccpars2_257(S, '@', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_257(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_257(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_258/7}).
yeccpars2_258(S, '$', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 85, Ss, Stack, T, Ts, Tzr);
yeccpars2_258(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_259/7}).
yeccpars2_259(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 266, Ss, Stack, T, Ts, Tzr);
yeccpars2_259(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_260(S, '$', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 85, Ss, Stack, T, Ts, Tzr);
yeccpars2_260(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_260_(Stack),
 'yeccgoto_\'VariableDefinitionList\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_261/7}).
yeccpars2_261(S, ':', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 262, Ss, Stack, T, Ts, Tzr);
yeccpars2_261(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_262: see yeccpars2_157

yeccpars2_263(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 177, Ss, Stack, T, Ts, Tzr);
yeccpars2_263(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_263_(Stack),
 'yeccgoto_\'VariableDefinition\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_264(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_264_(Stack),
 'yeccgoto_\'VariableDefinition\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_265(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_265_(Stack),
 'yeccgoto_\'VariableDefinitionList\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_266(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_266_(Stack),
 'yeccgoto_\'VariableDefinitions\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_267: see yeccpars2_64

yeccpars2_268(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_268_(Stack),
 'yeccgoto_\'OperationDefinition\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_269: see yeccpars2_60

yeccpars2_270(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_270_(Stack),
 'yeccgoto_\'OperationDefinition\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_271(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_271_(Stack),
 'yeccgoto_\'OperationDefinition\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_272: see yeccpars2_60

yeccpars2_273(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_273_(Stack),
 'yeccgoto_\'OperationDefinition\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_274(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_274_(Stack),
 'yeccgoto_\'OperationDefinition\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_275: see yeccpars2_60

yeccpars2_276(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_276_(Stack),
 'yeccgoto_\'OperationDefinition\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'Alias\''/7}).
'yeccgoto_\'Alias\''(31, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(39, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Alias\''(33, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(39, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'Argument\''/7}).
'yeccgoto_\'Argument\''(73, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(76, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Argument\''(76, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(76, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'ArgumentList\''/7}).
'yeccgoto_\'ArgumentList\''(73, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_75(75, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'ArgumentList\''(76=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'Arguments\''/7}).
'yeccgoto_\'Arguments\''(35, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_127(127, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Arguments\''(70=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_113(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Arguments\''(71=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_72(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Arguments\''(117, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_120(120, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'ArgumentsDefinition\''/7}).
'yeccgoto_\'ArgumentsDefinition\''(149, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_155(155, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'ArgumentsDefinition\''(243, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_244(244, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'DefaultValue\''/7}).
'yeccgoto_\'DefaultValue\''(174, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_176(176, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'DefaultValue\''(263=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_264(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'Definition\''/7}).
'yeccgoto_\'Definition\''(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(17, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Definition\''(17, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(17, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'Definitions\''/7}).
'yeccgoto_\'Definitions\''(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Definitions\''(17=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_254(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'Directive\''/7}).
'yeccgoto_\'Directive\''(27, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(61, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Directive\''(35, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(61, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Directive\''(40, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(61, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Directive\''(59, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(61, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Directive\''(61, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(61, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Directive\''(64, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(61, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Directive\''(117, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(61, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Directive\''(120, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(61, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Directive\''(127, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(61, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Directive\''(135, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(61, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Directive\''(144, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(61, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Directive\''(145, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(61, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Directive\''(149, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(61, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Directive\''(158, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(61, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Directive\''(174, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(61, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Directive\''(176, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(61, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Directive\''(181, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(61, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Directive\''(205, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(61, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Directive\''(207, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(61, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Directive\''(215, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(61, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Directive\''(225, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(61, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Directive\''(230, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(61, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Directive\''(235, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(61, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Directive\''(247, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(61, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Directive\''(252, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(61, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Directive\''(255, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(61, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Directive\''(257, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(61, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Directive\''(267, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(61, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'DirectiveDefinition\''/7}).
'yeccgoto_\'DirectiveDefinition\''(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'DirectiveDefinition\''(17=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'DirectiveDefinitionLocations\''/7}).
'yeccgoto_\'DirectiveDefinitionLocations\''(245, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_247(247, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'DirectiveDefinitionLocations\''(249=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_250(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'DirectiveDefinitionLocations\''(251, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_252(252, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'Directives\''/7}).
'yeccgoto_\'Directives\''(27, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_198(198, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Directives\''(35, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_126(126, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Directives\''(40, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(60, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Directives\''(59=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_116(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Directives\''(61=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Directives\''(64, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(68, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Directives\''(117, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_119(119, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Directives\''(120, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_122(122, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Directives\''(127, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_130(130, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Directives\''(135, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_136(136, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Directives\''(144, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_146(146, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Directives\''(145, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_191(191, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Directives\''(149, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(154, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Directives\''(158=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_167(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Directives\''(174=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_175(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Directives\''(176=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_179(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Directives\''(181=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_182(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Directives\''(205=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_206(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Directives\''(207, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_208(208, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Directives\''(215, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_216(216, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Directives\''(225, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(227, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Directives\''(230, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_231(231, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Directives\''(235=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_236(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Directives\''(247=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_248(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Directives\''(252=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_253(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Directives\''(255, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(275, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Directives\''(257, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(269, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Directives\''(267, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(272, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'Document\''/7}).
'yeccgoto_\'Document\''(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_14(14, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'EnumTypeDefinition\''/7}).
'yeccgoto_\'EnumTypeDefinition\''(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'EnumTypeDefinition\''(17=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'EnumValue\''/7}).
'yeccgoto_\'EnumValue\''(78=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_84(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'EnumValue\''(86=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_84(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'EnumValue\''(98=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_84(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'EnumValue\''(103=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_84(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'EnumValue\''(111=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_84(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'EnumValue\''(177=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_84(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'EnumValue\''(232, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_235(235, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'EnumValue\''(234, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_235(235, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'EnumValue\''(239, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_235(235, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'EnumValueDefinition\''/7}).
'yeccgoto_\'EnumValueDefinition\''(232, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_234(234, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'EnumValueDefinition\''(234, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_234(234, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'EnumValueDefinition\''(239, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_234(234, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'EnumValueDefinitionList\''/7}).
'yeccgoto_\'EnumValueDefinitionList\''(232, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_233(233, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'EnumValueDefinitionList\''(234=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_237(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'EnumValueDefinitionList\''(239, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_240(240, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'Field\''/7}).
'yeccgoto_\'Field\''(31=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_38(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Field\''(33=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_38(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'FieldDefinition\''/7}).
'yeccgoto_\'FieldDefinition\''(148, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(151, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'FieldDefinition\''(151, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(151, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'FieldDefinition\''(188, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(151, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'FieldDefinition\''(192, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(151, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'FieldDefinition\''(195, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(151, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'FieldDefinition\''(199, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(151, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'FieldDefinition\''(202, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(151, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'FieldDefinition\''(209, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(151, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'FieldDefinition\''(212, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(151, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'FieldDefinitionList\''/7}).
'yeccgoto_\'FieldDefinitionList\''(148, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(150, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'FieldDefinitionList\''(151=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'FieldDefinitionList\''(188, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_189(189, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'FieldDefinitionList\''(192, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_193(193, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'FieldDefinitionList\''(195, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_196(196, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'FieldDefinitionList\''(199, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_200(200, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'FieldDefinitionList\''(202, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_203(203, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'FieldDefinitionList\''(209, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_210(210, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'FieldDefinitionList\''(212, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_213(213, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'Fragment\''/7}).
'yeccgoto_\'Fragment\''(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Fragment\''(17=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'FragmentName\''/7}).
'yeccgoto_\'FragmentName\''(21, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_223(223, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'FragmentName\''(40, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_59(59, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'FragmentSpread\''/7}).
'yeccgoto_\'FragmentSpread\''(31=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'FragmentSpread\''(33=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'ImplementsInterfaces\''/7}).
'yeccgoto_\'ImplementsInterfaces\''(144, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(145, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'InlineFragment\''/7}).
'yeccgoto_\'InlineFragment\''(31=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'InlineFragment\''(33=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'InputObjectTypeDefinition\''/7}).
'yeccgoto_\'InputObjectTypeDefinition\''(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'InputObjectTypeDefinition\''(17=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'InputValueDefinition\''/7}).
'yeccgoto_\'InputValueDefinition\''(156, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_170(170, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'InputValueDefinition\''(170, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_170(170, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'InputValueDefinition\''(217, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_170(170, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'InputValueDefinition\''(220, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_170(170, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'InputValueDefinitionList\''/7}).
'yeccgoto_\'InputValueDefinitionList\''(156, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_169(169, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'InputValueDefinitionList\''(170=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_171(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'InputValueDefinitionList\''(217, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_218(218, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'InputValueDefinitionList\''(220, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_221(221, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'InterfaceTypeDefinition\''/7}).
'yeccgoto_\'InterfaceTypeDefinition\''(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'InterfaceTypeDefinition\''(17=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'ListType\''/7}).
'yeccgoto_\'ListType\''(157, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_161(161, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'ListType\''(162, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_161(161, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'ListType\''(173, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_161(161, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'ListType\''(180, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_161(161, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'ListType\''(183, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_161(161, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'ListType\''(262, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_161(161, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'ListValue\''/7}).
'yeccgoto_\'ListValue\''(78=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_83(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'ListValue\''(86=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_83(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'ListValue\''(98=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_83(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'ListValue\''(103=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_83(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'ListValue\''(111=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_83(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'ListValue\''(177=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_83(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'Name\''/7}).
'yeccgoto_\'Name\''(7, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_257(257, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Name\''(19, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_230(230, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Name\''(22, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_215(215, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Name\''(23, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_207(207, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Name\''(26, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_205(205, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Name\''(29, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(144, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Name\''(30, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_135(135, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Name\''(31, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(35, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Name\''(33, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(35, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Name\''(39, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_117(117, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Name\''(63=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Name\''(78=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Name\''(86=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Name\''(93, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(96, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Name\''(95, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(96, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Name\''(98=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Name\''(103=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Name\''(111=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Name\''(137=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Name\''(140=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Name\''(142=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Name\''(147=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Name\''(148, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(149, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Name\''(151, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(149, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Name\''(156, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_168(168, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Name\''(157=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Name\''(162=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Name\''(170, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_168(168, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Name\''(173=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Name\''(177=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Name\''(180=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Name\''(183=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Name\''(186=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Name\''(188, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(149, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Name\''(192, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(149, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Name\''(195, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(149, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Name\''(199, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(149, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Name\''(202, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(149, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Name\''(209, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(149, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Name\''(212, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(149, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Name\''(217, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_168(168, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Name\''(220, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_168(168, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Name\''(224=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Name\''(232=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Name\''(234=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Name\''(239=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Name\''(242, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_243(243, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Name\''(245, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_246(246, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Name\''(249, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_246(246, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Name\''(251, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_246(246, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Name\''(262=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'NameWithoutOn\''/7}).
'yeccgoto_\'NameWithoutOn\''(7=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NameWithoutOn\''(19=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NameWithoutOn\''(21=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_58(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NameWithoutOn\''(22=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NameWithoutOn\''(23=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NameWithoutOn\''(26=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NameWithoutOn\''(29=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NameWithoutOn\''(30=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NameWithoutOn\''(31=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NameWithoutOn\''(33=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NameWithoutOn\''(39=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NameWithoutOn\''(40=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_58(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NameWithoutOn\''(62, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_70(70, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NameWithoutOn\''(63=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NameWithoutOn\''(73, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_74(74, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NameWithoutOn\''(76, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_74(74, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NameWithoutOn\''(78=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NameWithoutOn\''(85=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_107(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NameWithoutOn\''(86=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NameWithoutOn\''(93=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NameWithoutOn\''(95=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NameWithoutOn\''(98=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NameWithoutOn\''(103=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NameWithoutOn\''(111=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NameWithoutOn\''(137=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NameWithoutOn\''(140=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NameWithoutOn\''(142=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NameWithoutOn\''(147=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NameWithoutOn\''(148=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NameWithoutOn\''(151=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NameWithoutOn\''(156=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NameWithoutOn\''(157=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NameWithoutOn\''(162=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NameWithoutOn\''(170=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NameWithoutOn\''(173=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NameWithoutOn\''(177=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NameWithoutOn\''(180=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NameWithoutOn\''(183=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NameWithoutOn\''(186=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NameWithoutOn\''(188=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NameWithoutOn\''(192=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NameWithoutOn\''(195=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NameWithoutOn\''(199=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NameWithoutOn\''(202=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NameWithoutOn\''(209=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NameWithoutOn\''(212=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NameWithoutOn\''(217=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NameWithoutOn\''(220=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NameWithoutOn\''(224=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NameWithoutOn\''(232=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NameWithoutOn\''(234=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NameWithoutOn\''(239=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NameWithoutOn\''(242=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NameWithoutOn\''(245=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NameWithoutOn\''(249=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NameWithoutOn\''(251=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NameWithoutOn\''(262=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'NamedType\''/7}).
'yeccgoto_\'NamedType\''(63=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NamedType\''(137, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_139(139, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NamedType\''(140, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_139(139, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NamedType\''(142, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_139(139, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NamedType\''(147, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_186(186, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NamedType\''(157, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(160, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NamedType\''(162, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(160, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NamedType\''(173, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(160, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NamedType\''(180, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(160, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NamedType\''(183, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(160, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NamedType\''(186, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_186(186, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NamedType\''(224=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NamedType\''(262, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(160, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'NamedTypeList\''/7}).
'yeccgoto_\'NamedTypeList\''(147=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_185(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NamedTypeList\''(186=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_187(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'NonNullType\''/7}).
'yeccgoto_\'NonNullType\''(157=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_159(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NonNullType\''(162=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_159(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NonNullType\''(173=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_159(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NonNullType\''(180=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_159(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NonNullType\''(183=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_159(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NonNullType\''(262=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_159(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'ObjectField\''/7}).
'yeccgoto_\'ObjectField\''(93, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_95(95, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'ObjectField\''(95, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_95(95, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'ObjectFields\''/7}).
'yeccgoto_\'ObjectFields\''(93, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_94(94, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'ObjectFields\''(95=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_100(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'ObjectTypeDefinition\''/7}).
'yeccgoto_\'ObjectTypeDefinition\''(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'ObjectTypeDefinition\''(17=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'ObjectTypeDefinition\''(20=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_229(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'ObjectValue\''/7}).
'yeccgoto_\'ObjectValue\''(78=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_81(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'ObjectValue\''(86=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_81(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'ObjectValue\''(98=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_81(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'ObjectValue\''(103=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_81(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'ObjectValue\''(111=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_81(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'ObjectValue\''(177=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_81(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'OperationDefinition\''/7}).
'yeccgoto_\'OperationDefinition\''(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'OperationDefinition\''(17=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'OperationType\''/7}).
'yeccgoto_\'OperationType\''(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(7, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'OperationType\''(17, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(7, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'ScalarTypeDefinition\''/7}).
'yeccgoto_\'ScalarTypeDefinition\''(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'ScalarTypeDefinition\''(17=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'SchemaDefinition\''/7}).
'yeccgoto_\'SchemaDefinition\''(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'SchemaDefinition\''(17=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'Selection\''/7}).
'yeccgoto_\'Selection\''(31, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(33, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Selection\''(33, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(33, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'SelectionSet\''/7}).
'yeccgoto_\'SelectionSet\''(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'SelectionSet\''(7=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_256(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'SelectionSet\''(17=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'SelectionSet\''(35=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_125(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'SelectionSet\''(40=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_57(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'SelectionSet\''(60=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'SelectionSet\''(64=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'SelectionSet\''(68=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_69(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'SelectionSet\''(117=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_118(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'SelectionSet\''(119=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_124(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'SelectionSet\''(120=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_121(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'SelectionSet\''(122=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_123(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'SelectionSet\''(126=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_132(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'SelectionSet\''(127=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_129(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'SelectionSet\''(130=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_131(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'SelectionSet\''(225=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_226(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'SelectionSet\''(227=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_228(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'SelectionSet\''(255=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_274(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'SelectionSet\''(257=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_268(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'SelectionSet\''(267=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_271(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'SelectionSet\''(269=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_270(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'SelectionSet\''(272=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_273(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'SelectionSet\''(275=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_276(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'Selections\''/7}).
'yeccgoto_\'Selections\''(31, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Selections\''(33=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_133(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'Type\''/7}).
'yeccgoto_\'Type\''(157, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(158, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Type\''(162, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_163(163, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Type\''(173, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_174(174, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Type\''(180, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_181(181, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Type\''(183=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_184(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Type\''(262, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_263(263, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'TypeCondition\''/7}).
'yeccgoto_\'TypeCondition\''(63, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(64, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'TypeCondition\''(224, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(225, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'TypeDefinition\''/7}).
'yeccgoto_\'TypeDefinition\''(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'TypeDefinition\''(17=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'TypeExtensionDefinition\''/7}).
'yeccgoto_\'TypeExtensionDefinition\''(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'TypeExtensionDefinition\''(17=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'UnionMembers\''/7}).
'yeccgoto_\'UnionMembers\''(137=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_138(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'UnionMembers\''(140=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_141(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'UnionMembers\''(142=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_143(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'UnionTypeDefinition\''/7}).
'yeccgoto_\'UnionTypeDefinition\''(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'UnionTypeDefinition\''(17=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'Value\''/7}).
'yeccgoto_\'Value\''(78=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_80(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Value\''(86, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_103(103, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Value\''(98=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_99(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Value\''(103, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_103(103, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Value\''(111=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_112(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Value\''(177=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_178(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'Values\''/7}).
'yeccgoto_\'Values\''(86, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_102(102, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Values\''(103=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_105(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'Variable\''/7}).
'yeccgoto_\'Variable\''(78=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Variable\''(86=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Variable\''(98=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Variable\''(103=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Variable\''(111=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Variable\''(177=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Variable\''(258, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_261(261, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Variable\''(260, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_261(261, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'VariableDefinition\''/7}).
'yeccgoto_\'VariableDefinition\''(258, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_260(260, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'VariableDefinition\''(260, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_260(260, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'VariableDefinitionList\''/7}).
'yeccgoto_\'VariableDefinitionList\''(258, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_259(259, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'VariableDefinitionList\''(260=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_265(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'VariableDefinitions\''/7}).
'yeccgoto_\'VariableDefinitions\''(7, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(255, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'VariableDefinitions\''(257, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(267, Cat, Ss, Stack, T, Ts, Tzr).

-compile({inline,yeccpars2_4_/1}).
-file("src/absinthe_parser.yrl", 35).
yeccpars2_4_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'OperationDefinition' , # { operation => query , selection_set => __1 } , # { start_line => extract_child_line ( __1 ) } )
  end | __Stack].

-compile({inline,yeccpars2_16_/1}).
-file("src/absinthe_parser.yrl", 22).
yeccpars2_16_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'Document' , # { definitions => __1 } , # { start_line => extract_line ( __1 ) } )
  end | __Stack].

-compile({inline,yeccpars2_17_/1}).
-file("src/absinthe_parser.yrl", 24).
yeccpars2_17_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_24_/1}).
-file("src/absinthe_parser.yrl", 32).
yeccpars2_24_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   extract_atom ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_25_/1}).
-file("src/absinthe_parser.yrl", 31).
yeccpars2_25_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   extract_atom ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_28_/1}).
-file("src/absinthe_parser.yrl", 33).
yeccpars2_28_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   extract_atom ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_33_/1}).
-file("src/absinthe_parser.yrl", 69).
yeccpars2_33_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_35_/1}).
-file("src/absinthe_parser.yrl", 86).
yeccpars2_35_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'Field' , # { name => extract_binary ( __1 ) } , # { start_line => extract_line ( __1 ) } )
  end | __Stack].

-compile({inline,yeccpars2_41_/1}).
-file("src/absinthe_parser.yrl", 132).
yeccpars2_41_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   extract_binary ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_42_/1}).
-file("src/absinthe_parser.yrl", 129).
yeccpars2_42_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   extract_binary ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_43_/1}).
-file("src/absinthe_parser.yrl", 131).
yeccpars2_43_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   extract_binary ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_44_/1}).
-file("src/absinthe_parser.yrl", 122).
yeccpars2_44_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   extract_binary ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_45_/1}).
-file("src/absinthe_parser.yrl", 124).
yeccpars2_45_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   extract_binary ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_46_/1}).
-file("src/absinthe_parser.yrl", 130).
yeccpars2_46_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   extract_binary ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_47_/1}).
-file("src/absinthe_parser.yrl", 125).
yeccpars2_47_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   extract_binary ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_48_/1}).
-file("src/absinthe_parser.yrl", 120).
yeccpars2_48_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   extract_binary ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_50_/1}).
-file("src/absinthe_parser.yrl", 135).
yeccpars2_50_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   extract_binary ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_51_/1}).
-file("src/absinthe_parser.yrl", 119).
yeccpars2_51_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   extract_binary ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_52_/1}).
-file("src/absinthe_parser.yrl", 127).
yeccpars2_52_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   extract_binary ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_53_/1}).
-file("src/absinthe_parser.yrl", 128).
yeccpars2_53_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   extract_binary ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_54_/1}).
-file("src/absinthe_parser.yrl", 121).
yeccpars2_54_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   extract_binary ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_55_/1}).
-file("src/absinthe_parser.yrl", 123).
yeccpars2_55_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   extract_binary ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_56_/1}).
-file("src/absinthe_parser.yrl", 126).
yeccpars2_56_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   extract_binary ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_57_/1}).
-file("src/absinthe_parser.yrl", 82).
yeccpars2_57_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'InlineFragment' , # { selection_set => __2 } , # { start_line => extract_line ( __1 ) } )
  end | __Stack].

-compile({inline,yeccpars2_58_/1}).
-file("src/absinthe_parser.yrl", 84).
yeccpars2_58_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   extract_binary ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_59_/1}).
-file("src/absinthe_parser.yrl", 76).
yeccpars2_59_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'FragmentSpread' , # { name => __2 } , # { start_line => extract_line ( __1 ) } )
  end | __Stack].

-compile({inline,yeccpars2_61_/1}).
-file("src/absinthe_parser.yrl", 111).
yeccpars2_61_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_66_/1}).
-file("src/absinthe_parser.yrl", 62).
yeccpars2_66_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'NamedType' , # { name => extract_binary ( __1 ) } , # { start_line => extract_line ( __1 ) } )
  end | __Stack].

-compile({inline,yeccpars2_67_/1}).
-file("src/absinthe_parser.yrl", 79).
yeccpars2_67_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'InlineFragment' , # { type_condition => __3 , selection_set => __4 } , # { start_line => extract_line ( __1 ) } )
  end | __Stack].

-compile({inline,yeccpars2_69_/1}).
-file("src/absinthe_parser.yrl", 80).
yeccpars2_69_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'InlineFragment' , # { type_condition => __3 , directives => __4 , selection_set => __5 } , # { start_line => extract_line ( __1 ) } )
  end | __Stack].

-compile({inline,yeccpars2_70_/1}).
-file("src/absinthe_parser.yrl", 113).
yeccpars2_70_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'Directive' , # { name => extract_binary ( __2 ) } , # { start_line => extract_line ( __1 ) } )
  end | __Stack].

-compile({inline,yeccpars2_71_/1}).
-file("src/absinthe_parser.yrl", 115).
yeccpars2_71_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'Directive' , # { name => extract_binary ( __2 ) } , # { start_line => extract_line ( __1 ) } )
  end | __Stack].

-compile({inline,yeccpars2_72_/1}).
-file("src/absinthe_parser.yrl", 116).
yeccpars2_72_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'Directive' , # { name => extract_binary ( __2 ) , arguments => __3 } , # { start_line => extract_line ( __1 ) } )
  end | __Stack].

-compile({inline,yeccpars2_76_/1}).
-file("src/absinthe_parser.yrl", 106).
yeccpars2_76_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_80_/1}).
-file("src/absinthe_parser.yrl", 109).
yeccpars2_80_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'Argument' , # { name => extract_binary ( __1 ) , value => __3 } , # { start_line => extract_line ( __1 ) } )
  end | __Stack].

-compile({inline,yeccpars2_81_/1}).
-file("src/absinthe_parser.yrl", 146).
yeccpars2_81_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'ObjectValue' , # { fields => __1 } , # { start_line => extract_child_line ( __1 ) } )
  end | __Stack].

-compile({inline,yeccpars2_82_/1}).
-file("src/absinthe_parser.yrl", 149).
yeccpars2_82_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   extract_binary ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_83_/1}).
-file("src/absinthe_parser.yrl", 145).
yeccpars2_83_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'ListValue' , # { values => __1 } , # { start_line => extract_child_line ( __1 ) } )
  end | __Stack].

-compile({inline,yeccpars2_84_/1}).
-file("src/absinthe_parser.yrl", 144).
yeccpars2_84_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'EnumValue' , # { value => __1 } , # { start_line => extract_line ( __1 ) } )
  end | __Stack].

-compile({inline,yeccpars2_87_/1}).
-file("src/absinthe_parser.yrl", 140).
yeccpars2_87_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'StringValue' , # { value => extract_quoted_block_string_token ( __1 ) } , # { start_line => extract_line ( __1 ) } )
  end | __Stack].

-compile({inline,yeccpars2_88_/1}).
-file("src/absinthe_parser.yrl", 142).
yeccpars2_88_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'BooleanValue' , # { value => extract_boolean ( __1 ) } , # { start_line => extract_line ( __1 ) } )
  end | __Stack].

-compile({inline,yeccpars2_89_/1}).
-file("src/absinthe_parser.yrl", 139).
yeccpars2_89_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'FloatValue' , # { value => extract_float ( __1 ) } , # { start_line => extract_line ( __1 ) } )
  end | __Stack].

-compile({inline,yeccpars2_90_/1}).
-file("src/absinthe_parser.yrl", 138).
yeccpars2_90_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'IntValue' , # { value => extract_integer ( __1 ) } , # { start_line => extract_line ( __1 ) } )
  end | __Stack].

-compile({inline,yeccpars2_91_/1}).
-file("src/absinthe_parser.yrl", 143).
yeccpars2_91_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'NullValue' , # { } , # { start_line => extract_line ( __1 ) } )
  end | __Stack].

-compile({inline,yeccpars2_92_/1}).
-file("src/absinthe_parser.yrl", 141).
yeccpars2_92_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'StringValue' , # { value => extract_quoted_string_token ( __1 ) } , # { start_line => extract_line ( __1 ) } )
  end | __Stack].

-compile({inline,yeccpars2_95_/1}).
-file("src/absinthe_parser.yrl", 158).
yeccpars2_95_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_97_/1}).
-file("src/absinthe_parser.yrl", 156).
yeccpars2_97_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ ]
  end | __Stack].

-compile({inline,yeccpars2_99_/1}).
-file("src/absinthe_parser.yrl", 160).
yeccpars2_99_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'ObjectField' , # { name => extract_binary ( __1 ) , value => __3 } , # { start_line => extract_line ( __1 ) } )
  end | __Stack].

-compile({inline,yeccpars2_100_/1}).
-file("src/absinthe_parser.yrl", 159).
yeccpars2_100_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,yeccpars2_101_/1}).
-file("src/absinthe_parser.yrl", 157).
yeccpars2_101_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_103_/1}).
-file("src/absinthe_parser.yrl", 153).
yeccpars2_103_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_104_/1}).
-file("src/absinthe_parser.yrl", 151).
yeccpars2_104_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ ]
  end | __Stack].

-compile({inline,yeccpars2_105_/1}).
-file("src/absinthe_parser.yrl", 154).
yeccpars2_105_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,yeccpars2_106_/1}).
-file("src/absinthe_parser.yrl", 152).
yeccpars2_106_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_107_/1}).
-file("src/absinthe_parser.yrl", 54).
yeccpars2_107_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'Variable' , # { name => extract_binary ( __2 ) } , # { start_line => extract_line ( __1 ) } )
  end | __Stack].

-compile({inline,yeccpars2_108_/1}).
-file("src/absinthe_parser.yrl", 55).
yeccpars2_108_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'Variable' , # { name => extract_binary ( __2 ) } , # { start_line => extract_line ( __1 ) } )
  end | __Stack].

-compile({inline,yeccpars2_109_/1}).
-file("src/absinthe_parser.yrl", 107).
yeccpars2_109_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,yeccpars2_110_/1}).
-file("src/absinthe_parser.yrl", 105).
yeccpars2_110_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_112_/1}).
-file("src/absinthe_parser.yrl", 108).
yeccpars2_112_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'Argument' , # { name => extract_binary ( __1 ) , value => __3 } , # { start_line => extract_line ( __1 ) } )
  end | __Stack].

-compile({inline,yeccpars2_113_/1}).
-file("src/absinthe_parser.yrl", 114).
yeccpars2_113_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'Directive' , # { name => extract_binary ( __2 ) , arguments => __3 } , # { start_line => extract_line ( __1 ) } )
  end | __Stack].

-compile({inline,yeccpars2_114_/1}).
-file("src/absinthe_parser.yrl", 112).
yeccpars2_114_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,yeccpars2_115_/1}).
-file("src/absinthe_parser.yrl", 81).
yeccpars2_115_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'InlineFragment' , # { directives => __2 , selection_set => __3 } , # { start_line => extract_line ( __1 ) } )
  end | __Stack].

-compile({inline,yeccpars2_116_/1}).
-file("src/absinthe_parser.yrl", 77).
yeccpars2_116_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'FragmentSpread' , # { name => __2 , directives => __3 } , # { start_line => extract_line ( __1 ) } )
  end | __Stack].

-compile({inline,yeccpars2_117_/1}).
-file("src/absinthe_parser.yrl", 94).
yeccpars2_117_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'Field' , # { alias => extract_binary ( __1 ) , name => extract_binary ( __2 ) } , # { start_line => extract_line ( __1 ) } )
  end | __Stack].

-compile({inline,yeccpars2_118_/1}).
-file("src/absinthe_parser.yrl", 96).
yeccpars2_118_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'Field' , # { alias => extract_binary ( __1 ) , name => extract_binary ( __2 ) , selection_set => __3 } , # { start_line => extract_line ( __1 ) } )
  end | __Stack].

-compile({inline,yeccpars2_119_/1}).
-file("src/absinthe_parser.yrl", 98).
yeccpars2_119_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'Field' , # { alias => extract_binary ( __1 ) , name => extract_binary ( __2 ) , directives => __3 } , # { start_line => extract_line ( __1 ) } )
  end | __Stack].

-compile({inline,yeccpars2_120_/1}).
-file("src/absinthe_parser.yrl", 95).
yeccpars2_120_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'Field' , # { alias => extract_binary ( __1 ) , name => extract_binary ( __2 ) , arguments => __3 } , # { start_line => extract_line ( __1 ) } )
  end | __Stack].

-compile({inline,yeccpars2_121_/1}).
-file("src/absinthe_parser.yrl", 97).
yeccpars2_121_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'Field' , # { alias => extract_binary ( __1 ) , name => extract_binary ( __2 ) , arguments => __3 , selection_set => __4 } , # { start_line => extract_line ( __1 ) } )
  end | __Stack].

-compile({inline,yeccpars2_122_/1}).
-file("src/absinthe_parser.yrl", 99).
yeccpars2_122_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'Field' , # { alias => extract_binary ( __1 ) , name => extract_binary ( __2 ) , arguments => __3 , directives => __4 } , # { start_line => extract_line ( __1 ) } )
  end | __Stack].

-compile({inline,yeccpars2_123_/1}).
-file("src/absinthe_parser.yrl", 101).
yeccpars2_123_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'Field' , # { alias => extract_binary ( __1 ) , name => extract_binary ( __2 ) , arguments => __3 , directives => __4 , selection_set => __5 } , # { start_line => extract_line ( __1 ) } )
  end | __Stack].

-compile({inline,yeccpars2_124_/1}).
-file("src/absinthe_parser.yrl", 100).
yeccpars2_124_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'Field' , # { alias => extract_binary ( __1 ) , name => extract_binary ( __2 ) , directives => __3 , selection_set => __4 } , # { start_line => extract_line ( __1 ) } )
  end | __Stack].

-compile({inline,yeccpars2_125_/1}).
-file("src/absinthe_parser.yrl", 89).
yeccpars2_125_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'Field' , # { name => extract_binary ( __1 ) , selection_set => __2 } , # { start_line => extract_line ( __1 ) } )
  end | __Stack].

-compile({inline,yeccpars2_126_/1}).
-file("src/absinthe_parser.yrl", 88).
yeccpars2_126_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'Field' , # { name => extract_binary ( __1 ) , directives => __2 } , # { start_line => extract_line ( __1 ) } )
  end | __Stack].

-compile({inline,yeccpars2_127_/1}).
-file("src/absinthe_parser.yrl", 87).
yeccpars2_127_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'Field' , # { name => extract_binary ( __1 ) , arguments => __2 } , # { start_line => extract_line ( __1 ) } )
  end | __Stack].

-compile({inline,yeccpars2_128_/1}).
-file("src/absinthe_parser.yrl", 103).
yeccpars2_128_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1
  end | __Stack].

-compile({inline,yeccpars2_129_/1}).
-file("src/absinthe_parser.yrl", 91).
yeccpars2_129_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'Field' , # { name => extract_binary ( __1 ) , arguments => __2 , selection_set => __3 } , # { start_line => extract_line ( __1 ) } )
  end | __Stack].

-compile({inline,yeccpars2_130_/1}).
-file("src/absinthe_parser.yrl", 92).
yeccpars2_130_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'Field' , # { name => extract_binary ( __1 ) , arguments => __2 , directives => __3 } , # { start_line => extract_line ( __1 ) } )
  end | __Stack].

-compile({inline,yeccpars2_131_/1}).
-file("src/absinthe_parser.yrl", 93).
yeccpars2_131_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'Field' , # { name => extract_binary ( __1 ) , arguments => __2 , directives => __3 , selection_set => __4 } , # { start_line => extract_line ( __1 ) } )
  end | __Stack].

-compile({inline,yeccpars2_132_/1}).
-file("src/absinthe_parser.yrl", 90).
yeccpars2_132_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'Field' , # { name => extract_binary ( __1 ) , directives => __2 , selection_set => __3 } , # { start_line => extract_line ( __1 ) } )
  end | __Stack].

-compile({inline,yeccpars2_133_/1}).
-file("src/absinthe_parser.yrl", 70).
yeccpars2_133_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,yeccpars2_134_/1}).
-file("src/absinthe_parser.yrl", 67).
yeccpars2_134_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'SelectionSet' , # { selections => __2 } , # { start_line => extract_line ( __1 ) , end_line => extract_line ( __3 ) } )
  end | __Stack].

-compile({inline,yeccpars2_138_/1}).
-file("src/absinthe_parser.yrl", 223).
yeccpars2_138_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'UnionTypeDefinition' , # { name => extract_binary ( __2 ) , types => __4 } , # { start_line => extract_line ( __1 ) } )
  end | __Stack].

-compile({inline,yeccpars2_139_/1}).
-file("src/absinthe_parser.yrl", 227).
yeccpars2_139_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_141_/1}).
-file("src/absinthe_parser.yrl", 228).
yeccpars2_141_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,yeccpars2_143_/1}).
-file("src/absinthe_parser.yrl", 225).
yeccpars2_143_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'UnionTypeDefinition' , # { name => extract_binary ( __2 ) , directives => __3 , types => __5 } , # { start_line => extract_line ( __1 ) } )
  end | __Stack].

-compile({inline,yeccpars2_151_/1}).
-file("src/absinthe_parser.yrl", 199).
yeccpars2_151_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_152_/1}).
-file("src/absinthe_parser.yrl", 200).
yeccpars2_152_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,yeccpars2_153_/1}).
-file("src/absinthe_parser.yrl", 186).
yeccpars2_153_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'ObjectTypeDefinition' , # { name => extract_binary ( __2 ) , fields => __4 } , # { start_line => extract_line ( __1 ) , end_line => extract_line ( __5 ) } )
  end | __Stack].

-compile({inline,yeccpars2_158_/1}).
-file("src/absinthe_parser.yrl", 201).
yeccpars2_158_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'FieldDefinition' , # { name => extract_binary ( __1 ) , type => __3 } , # { start_line => extract_line ( __1 ) } )
  end | __Stack].

-compile({inline,yeccpars2_164_/1}).
-file("src/absinthe_parser.yrl", 63).
yeccpars2_164_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'ListType' , # { type => __2 } , # { start_line => extract_line ( __1 ) } )
  end | __Stack].

-compile({inline,yeccpars2_165_/1}).
-file("src/absinthe_parser.yrl", 65).
yeccpars2_165_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'NonNullType' , # { type => __1 } , # { start_line => extract_line ( __1 ) } )
  end | __Stack].

-compile({inline,yeccpars2_166_/1}).
-file("src/absinthe_parser.yrl", 64).
yeccpars2_166_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'NonNullType' , # { type => __1 } , # { start_line => extract_line ( __1 ) } )
  end | __Stack].

-compile({inline,yeccpars2_167_/1}).
-file("src/absinthe_parser.yrl", 202).
yeccpars2_167_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'FieldDefinition' , # { name => extract_binary ( __1 ) , type => __3 , directives => __4 } , # { start_line => extract_line ( __1 ) } )
  end | __Stack].

-compile({inline,yeccpars2_170_/1}).
-file("src/absinthe_parser.yrl", 209).
yeccpars2_170_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_171_/1}).
-file("src/absinthe_parser.yrl", 210).
yeccpars2_171_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,yeccpars2_172_/1}).
-file("src/absinthe_parser.yrl", 207).
yeccpars2_172_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_174_/1}).
-file("src/absinthe_parser.yrl", 212).
yeccpars2_174_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'InputValueDefinition' , # { name => extract_binary ( __1 ) , type => __3 } , # { start_line => extract_line ( __1 ) } )
  end | __Stack].

-compile({inline,yeccpars2_175_/1}).
-file("src/absinthe_parser.yrl", 213).
yeccpars2_175_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'InputValueDefinition' , # { name => extract_binary ( __1 ) , type => __3 , directives => __4 } , # { start_line => extract_line ( __1 ) } )
  end | __Stack].

-compile({inline,yeccpars2_176_/1}).
-file("src/absinthe_parser.yrl", 214).
yeccpars2_176_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'InputValueDefinition' , # { name => extract_binary ( __1 ) , type => __3 , default_value => __4 } , # { start_line => extract_line ( __1 ) } )
  end | __Stack].

-compile({inline,yeccpars2_178_/1}).
-file("src/absinthe_parser.yrl", 57).
yeccpars2_178_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_179_/1}).
-file("src/absinthe_parser.yrl", 215).
yeccpars2_179_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'InputValueDefinition' , # { name => extract_binary ( __1 ) , type => __3 , default_value => __4 , directives => __5 } , # { start_line => extract_line ( __1 ) } )
  end | __Stack].

-compile({inline,yeccpars2_181_/1}).
-file("src/absinthe_parser.yrl", 203).
yeccpars2_181_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'FieldDefinition' , # { name => extract_binary ( __1 ) , arguments => __2 , type => __4 } , # { start_line => extract_line ( __1 ) } )
  end | __Stack].

-compile({inline,yeccpars2_182_/1}).
-file("src/absinthe_parser.yrl", 205).
yeccpars2_182_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'FieldDefinition' , # { name => extract_binary ( __1 ) , arguments => __2 , directives => __5 , type => __4 } , # { start_line => extract_line ( __1 ) } )
  end | __Stack].

-compile({inline,yeccpars2_184_/1}).
-file("src/absinthe_parser.yrl", 204).
yeccpars2_184_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'FieldDefinition' , # { name => extract_binary ( __1 ) , directives => __2 , type => __4 } , # { start_line => extract_line ( __1 ) } )
  end | __Stack].

-compile({inline,yeccpars2_185_/1}).
-file("src/absinthe_parser.yrl", 194).
yeccpars2_185_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_186_/1}).
-file("src/absinthe_parser.yrl", 196).
yeccpars2_186_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_187_/1}).
-file("src/absinthe_parser.yrl", 197).
yeccpars2_187_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,yeccpars2_190_/1}).
-file("src/absinthe_parser.yrl", 188).
yeccpars2_190_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'ObjectTypeDefinition' , # { name => extract_binary ( __2 ) , directives => __3 , fields => __5 } , # { start_line => extract_line ( __1 ) , end_line => extract_line ( __6 ) } )
  end | __Stack].

-compile({inline,yeccpars2_194_/1}).
-file("src/absinthe_parser.yrl", 190).
yeccpars2_194_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'ObjectTypeDefinition' , # { name => extract_binary ( __2 ) , interfaces => __3 , fields => __5 } , # { start_line => extract_line ( __1 ) , end_line => extract_line ( __6 ) } )
  end | __Stack].

-compile({inline,yeccpars2_197_/1}).
-file("src/absinthe_parser.yrl", 192).
yeccpars2_197_(__Stack0) ->
 [__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'ObjectTypeDefinition' , # { name => extract_binary ( __2 ) , interfaces => __3 , directives => __4 , fields => __6 } , # { start_line => extract_line ( __1 ) , end_line => extract_line ( __7 ) } )
  end | __Stack].

-compile({inline,yeccpars2_201_/1}).
-file("src/absinthe_parser.yrl", 182).
yeccpars2_201_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'SchemaDefinition' , # { fields => __3 } , # { start_line => extract_line ( __1 ) } )
  end | __Stack].

-compile({inline,yeccpars2_204_/1}).
-file("src/absinthe_parser.yrl", 183).
yeccpars2_204_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'SchemaDefinition' , # { directives => __2 , fields => __4 } , # { start_line => extract_line ( __1 ) } )
  end | __Stack].

-compile({inline,yeccpars2_205_/1}).
-file("src/absinthe_parser.yrl", 230).
yeccpars2_205_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'ScalarTypeDefinition' , # { name => extract_binary ( __2 ) } , # { start_line => extract_line ( __2 ) } )
  end | __Stack].

-compile({inline,yeccpars2_206_/1}).
-file("src/absinthe_parser.yrl", 231).
yeccpars2_206_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'ScalarTypeDefinition' , # { name => extract_binary ( __2 ) , directives => __3 } , # { start_line => extract_line ( __2 ) } )
  end | __Stack].

-compile({inline,yeccpars2_211_/1}).
-file("src/absinthe_parser.yrl", 218).
yeccpars2_211_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'InterfaceTypeDefinition' , # { name => extract_binary ( __2 ) , fields => __4 } , # { start_line => extract_line ( __1 ) , end_line => extract_line ( __5 ) } )
  end | __Stack].

-compile({inline,yeccpars2_214_/1}).
-file("src/absinthe_parser.yrl", 220).
yeccpars2_214_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'InterfaceTypeDefinition' , # { name => extract_binary ( __2 ) , directives => __3 , fields => __5 } , # { start_line => extract_line ( __1 ) , end_line => extract_line ( __6 ) } )
  end | __Stack].

-compile({inline,yeccpars2_219_/1}).
-file("src/absinthe_parser.yrl", 249).
yeccpars2_219_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'InputObjectTypeDefinition' , # { name => extract_binary ( __2 ) , fields => __4 } , # { start_line => extract_line ( __2 ) , end_line => extract_line ( __5 ) } )
  end | __Stack].

-compile({inline,yeccpars2_222_/1}).
-file("src/absinthe_parser.yrl", 251).
yeccpars2_222_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'InputObjectTypeDefinition' , # { name => extract_binary ( __2 ) , directives => __3 , fields => __5 } , # { start_line => extract_line ( __2 ) , end_line => extract_line ( __6 ) } )
  end | __Stack].

-compile({inline,yeccpars2_226_/1}).
-file("src/absinthe_parser.yrl", 44).
yeccpars2_226_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'Fragment' , # { name => __2 , type_condition => __4 , selection_set => __5 } , # { start_line => extract_line ( __1 ) } )
  end | __Stack].

-compile({inline,yeccpars2_228_/1}).
-file("src/absinthe_parser.yrl", 45).
yeccpars2_228_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'Fragment' , # { name => __2 , type_condition => __4 , directives => __5 , selection_set => __6 } , # { start_line => extract_line ( __1 ) } )
  end | __Stack].

-compile({inline,yeccpars2_229_/1}).
-file("src/absinthe_parser.yrl", 255).
yeccpars2_229_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'TypeExtensionDefinition' , # { definition => __2 } , # { start_line => extract_line ( __1 ) } )
  end | __Stack].

-compile({inline,yeccpars2_234_/1}).
-file("src/absinthe_parser.yrl", 238).
yeccpars2_234_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_235_/1}).
-file("src/absinthe_parser.yrl", 244).
yeccpars2_235_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'EnumValueDefinition' , # { value => extract_binary ( __1 ) } , # { start_line => extract_line ( __1 ) } )
  end | __Stack].

-compile({inline,yeccpars2_236_/1}).
-file("src/absinthe_parser.yrl", 245).
yeccpars2_236_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'EnumValueDefinition' , # { value => extract_binary ( __1 ) , directives => __2 } , # { start_line => extract_line ( __1 ) } )
  end | __Stack].

-compile({inline,yeccpars2_237_/1}).
-file("src/absinthe_parser.yrl", 239).
yeccpars2_237_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,yeccpars2_238_/1}).
-file("src/absinthe_parser.yrl", 234).
yeccpars2_238_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'EnumTypeDefinition' , # { name => extract_binary ( __2 ) , values => __4 } , # { start_line => extract_line ( __2 ) , end_line => extract_line ( __5 ) } )
  end | __Stack].

-compile({inline,yeccpars2_241_/1}).
-file("src/absinthe_parser.yrl", 236).
yeccpars2_241_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'EnumTypeDefinition' , # { name => extract_binary ( __2 ) , directives => __3 , values => __5 } , # { start_line => extract_line ( __2 ) , end_line => extract_line ( __6 ) } )
  end | __Stack].

-compile({inline,yeccpars2_246_/1}).
-file("src/absinthe_parser.yrl", 241).
yeccpars2_246_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ extract_binary ( __1 ) ]
  end | __Stack].

-compile({inline,yeccpars2_247_/1}).
-file("src/absinthe_parser.yrl", 173).
yeccpars2_247_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'DirectiveDefinition' , # { name => extract_binary ( __3 ) , locations => __5 } , # { start_line => extract_line ( __1 ) } )
  end | __Stack].

-compile({inline,yeccpars2_248_/1}).
-file("src/absinthe_parser.yrl", 178).
yeccpars2_248_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'DirectiveDefinition' , # { name => extract_binary ( __3 ) , directives => __6 , locations => __5 } , # { start_line => extract_line ( __1 ) } )
  end | __Stack].

-compile({inline,yeccpars2_250_/1}).
-file("src/absinthe_parser.yrl", 242).
yeccpars2_250_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ extract_binary ( __1 ) | __3 ]
  end | __Stack].

-compile({inline,yeccpars2_252_/1}).
-file("src/absinthe_parser.yrl", 175).
yeccpars2_252_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'DirectiveDefinition' , # { name => extract_binary ( __3 ) , arguments => __4 , locations => __6 } , # { start_line => extract_line ( __1 ) , end_line => extract_line ( __1 ) } )
  end | __Stack].

-compile({inline,yeccpars2_253_/1}).
-file("src/absinthe_parser.yrl", 180).
yeccpars2_253_(__Stack0) ->
 [__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'DirectiveDefinition' , # { name => extract_binary ( __3 ) , arguments => __4 , directives => __7 , locations => __6 } , # { start_line => extract_line ( __1 ) , end_line => extract_line ( __1 ) } )
  end | __Stack].

-compile({inline,yeccpars2_254_/1}).
-file("src/absinthe_parser.yrl", 25).
yeccpars2_254_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,yeccpars2_256_/1}).
-file("src/absinthe_parser.yrl", 36).
yeccpars2_256_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'OperationDefinition' , # { operation => __1 , selection_set => __2 } , # { start_line => extract_line ( __2 ) } )
  end | __Stack].

-compile({inline,yeccpars2_260_/1}).
-file("src/absinthe_parser.yrl", 50).
yeccpars2_260_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_263_/1}).
-file("src/absinthe_parser.yrl", 52).
yeccpars2_263_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'VariableDefinition' , # { variable => __1 , type => __3 } , # { start_line => extract_child_line ( __1 ) } )
  end | __Stack].

-compile({inline,yeccpars2_264_/1}).
-file("src/absinthe_parser.yrl", 53).
yeccpars2_264_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'VariableDefinition' , # { variable => __1 , type => __3 , default_value => __4 } , # { start_line => extract_child_line ( __1 ) } )
  end | __Stack].

-compile({inline,yeccpars2_265_/1}).
-file("src/absinthe_parser.yrl", 51).
yeccpars2_265_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,yeccpars2_266_/1}).
-file("src/absinthe_parser.yrl", 49).
yeccpars2_266_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_268_/1}).
-file("src/absinthe_parser.yrl", 39).
yeccpars2_268_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'OperationDefinition' , # { operation => __1 , name => extract_binary ( __2 ) , selection_set => __3 } , # { start_line => extract_line ( __2 ) } )
  end | __Stack].

-compile({inline,yeccpars2_270_/1}).
-file("src/absinthe_parser.yrl", 41).
yeccpars2_270_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'OperationDefinition' , # { operation => __1 , name => extract_binary ( __2 ) , directives => __3 , selection_set => __4 } , # { start_line => extract_line ( __2 ) } )
  end | __Stack].

-compile({inline,yeccpars2_271_/1}).
-file("src/absinthe_parser.yrl", 40).
yeccpars2_271_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'OperationDefinition' , # { operation => __1 , name => extract_binary ( __2 ) , variable_definitions => __3 , selection_set => __4 } , # { start_line => extract_line ( __2 ) } )
  end | __Stack].

-compile({inline,yeccpars2_273_/1}).
-file("src/absinthe_parser.yrl", 42).
yeccpars2_273_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'OperationDefinition' , # { operation => __1 , name => extract_binary ( __2 ) , variable_definitions => __3 , directives => __4 , selection_set => __5 } , # { start_line => extract_line ( __2 ) } )
  end | __Stack].

-compile({inline,yeccpars2_274_/1}).
-file("src/absinthe_parser.yrl", 37).
yeccpars2_274_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'OperationDefinition' , # { operation => __1 , variable_definitions => __2 , selection_set => __3 } , # { start_line => extract_child_line ( __2 ) } )
  end | __Stack].

-compile({inline,yeccpars2_276_/1}).
-file("src/absinthe_parser.yrl", 38).
yeccpars2_276_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_ast_node ( 'OperationDefinition' , # { operation => __1 , variable_definitions => __2 , directives => __3 , selection_set => __4 } , # { start_line => extract_child_line ( __2 ) } )
  end | __Stack].


-file("src/absinthe_parser.yrl", 458).
