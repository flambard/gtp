-module(gtp_command_clear_board).
-behaviour(gtp_controller_command).
-behaviour(gtp_engine_command).
-include("gtp.hrl").

-export([
    command_name/0,
    encode_command_arguments/1,
    decode_command_arguments/1
]).

command_name() -> <<"clear_board">>.

encode_command_arguments(#clear_board{}) ->
    [].

decode_command_arguments(<<>>) ->
    #clear_board{}.
