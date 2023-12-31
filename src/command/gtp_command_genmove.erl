-module(gtp_command_genmove).
-behaviour(gtp_controller_command).
-include("gtp.hrl").

-export([
    command_name/0,
    encode_command_arguments/1,
    decode_response_values/1
]).

command_name() -> <<"genmove">>.

encode_command_arguments(#genmove{color = Color}) ->
    [gtp_entity:encode(color, Color)].

decode_response_values([Line]) ->
    {Vertex, []} = gtp_entity:decode(vertex, Line),
    #{vertex => Vertex}.
