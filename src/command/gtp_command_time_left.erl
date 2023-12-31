-module(gtp_command_time_left).
-behaviour(gtp_controller_command).
-behaviour(gtp_engine_command).
-include("gtp.hrl").

-export([
    command_name/0,
    encode_command_arguments/1,
    decode_command_arguments/1
]).

command_name() ->
    <<"time_left">>.

encode_command_arguments(#time_left{color = Color, time = Time, stones = Stones}) ->
    [gtp_entity:encode([color, int, int], [Color, Time, Stones])].

decode_command_arguments(Binary) ->
    {[Color, Time, Stones], []} = gtp_entity:decode([color, int, int], Binary),
    #time_left{
        color = Color,
        time = Time,
        stones = Stones
    }.
