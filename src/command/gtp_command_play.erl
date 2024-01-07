-module(gtp_command_play).
-behaviour(gtp_controller_command).
-behaviour(gtp_engine_command).
-include("gtp.hrl").

-export([
    command_name/0,
    encode_command_arguments/1,
    decode_command_arguments/1
]).

command_name() -> <<"play">>.

encode_command_arguments(#play{move = Move}) ->
    [gtp_entity:encode(move, Move)].

decode_command_arguments(Binary) ->
    #play{move = gtp_entity:decode_line(move, Binary)}.
