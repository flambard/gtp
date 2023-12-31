-module(gtp_command_fixed_handicap).
-behaviour(gtp_controller_command).
-include("gtp.hrl").

-export([
    command_name/0,
    encode_command_arguments/1,
    decode_response_values/1
]).

command_name() -> <<"fixed_handicap">>.

encode_command_arguments(#fixed_handicap{number_of_stones = Stones}) ->
    [gtp_entity:encode(int, Stones)].

decode_response_values([Line]) ->
    #{vertices => gtp_entity:decode_line({list, vertex}, Line)}.
