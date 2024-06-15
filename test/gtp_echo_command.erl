-module(gtp_echo_command).

-behaviour(gtp_controller_command).
-behaviour(gtp_engine_command).

-export([new/1]).

%% gtp_command callbacks
-export([
    command_name/0,
    decode_command_arguments/1,
    decode_response_values/1,
    encode_command_arguments/1,
    encode_response_values/1
]).

-record(echo, {value}).

new(Value) ->
    #echo{value = Value}.

command_name() ->
    <<"test-echo">>.

encode_command_arguments(#echo{value = Value}) ->
    [gtp_entity:encode(string, Value)].

decode_command_arguments(Binary) ->
    #echo{value = gtp_entity:decode_line(string, Binary)}.

encode_response_values(#{value := Value}) ->
    [gtp_entity:encode(string, Value)].

decode_response_values([BinValue]) ->
    #{value => gtp_entity:decode_line(string, BinValue)}.
