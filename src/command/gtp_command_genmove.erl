-module(gtp_command_genmove).
-behaviour(gtp_controller_command).
-behaviour(gtp_engine_command).
-include("gtp.hrl").

-export([
    command_name/0,
    encode_command_arguments/1,
    decode_command_arguments/1,
    encode_response_values/1,
    decode_response_values/1
]).

command_name() -> <<"genmove">>.

encode_command_arguments(#genmove{color = Color}) ->
    [gtp_entity:encode(color, Color)].

decode_command_arguments(Binary) ->
    #genmove{color = gtp_entity:decode_line(color, Binary)}.

encode_response_values(#{vertex := Vertex}) ->
    [gtp_entity:encode(vertex, Vertex)].

decode_response_values([Line]) ->
    #{vertex => gtp_entity:decode_line(vertex, Line)}.
