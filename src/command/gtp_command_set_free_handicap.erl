-module(gtp_command_set_free_handicap).
-behaviour(gtp_controller_command).
-behaviour(gtp_engine_command).
-include("gtp.hrl").

-export([
    command_name/0,
    encode_command_arguments/1,
    decode_command_arguments/1
]).

command_name() -> <<"set_free_handicap">>.

encode_command_arguments(#set_free_handicap{vertices = Vertices}) ->
    [gtp_entity:encode({list, vertex}, Vertices)].

decode_command_arguments(Binary) ->
    #set_free_handicap{vertices = gtp_entity:decode_line({list, vertex}, Binary)}.
