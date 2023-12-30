-module(gtp_command_play).
-behaviour(gtp_controller_command).
-include("gtp.hrl").

-export([
    command_name/0,
    encode_command_arguments/1
]).

command_name() -> <<"play">>.

encode_command_arguments(#play{move = Move}) ->
    [gtp_entity:encode_move(Move)].
