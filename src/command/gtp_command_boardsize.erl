-module(gtp_command_boardsize).
-behaviour(gtp_controller_command).
-behaviour(gtp_engine_command).
-include("gtp.hrl").

-export([
    command_name/0,
    encode_command_arguments/1,
    decode_command_arguments/1
]).

command_name() -> <<"boardsize">>.

encode_command_arguments(#boardsize{size = Size}) ->
    [gtp_entity:encode(int, Size)].

decode_command_arguments(Binary) ->
    #boardsize{size = gtp_entity:decode_line(int, Binary)}.
