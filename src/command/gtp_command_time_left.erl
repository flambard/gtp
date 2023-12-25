-module(gtp_command_time_left).
-behaviour(gtp_controller_command).
-behaviour(gtp_engine_command).
-include("gtp.hrl").

-export([
    name/0,
    encode_command_arguments/1,
    decode_command_arguments/1,
    encode_response_values/1,
    decode_response_values/1
]).

name() ->
    <<"time_left">>.

encode_command_arguments(#time_left{}) ->
    [].

decode_command_arguments(_Bin) ->
    #{}.

encode_response_values(#{}) ->
    [].

decode_response_values([]) ->
    #{}.
