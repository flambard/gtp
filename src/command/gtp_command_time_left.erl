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

encode_command_arguments(#time_left{color = Color, time = Time, stones = Stones}) ->
    [
        gtp_types:encode_color(Color),
        gtp_types:encode_int(Time),
        gtp_types:encode_int(Stones)
    ].

decode_command_arguments(Bin) ->
    {Color, [R1]} = gtp_types:decode_color(Bin),
    {Time, [R2]} = gtp_types:decode_int(R1),
    {Stones, []} = gtp_types:decode_int(R2),
    #time_left{
        color = Color,
        time = Time,
        stones = Stones
    }.

encode_response_values(#{}) ->
    [].

decode_response_values([]) ->
    #{}.
