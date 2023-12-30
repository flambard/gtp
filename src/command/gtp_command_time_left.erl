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
    [
        gtp_entity:encode_color(Color),
        gtp_entity:encode_int(Time),
        gtp_entity:encode_int(Stones)
    ].

decode_command_arguments(Bin) ->
    {Color, [R1]} = gtp_entity:decode_color(Bin),
    {Time, [R2]} = gtp_entity:decode_int(R1),
    {Stones, []} = gtp_entity:decode_int(R2),
    #time_left{
        color = Color,
        time = Time,
        stones = Stones
    }.
