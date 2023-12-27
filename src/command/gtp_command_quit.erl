-module(gtp_command_quit).
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

% arguments: none
% effects: The session is terminated and the connection closed.
% output: none
% fails: never

name() ->
    <<"quit">>.

encode_command_arguments(#quit{}) ->
    [].

decode_command_arguments(<<>>) ->
    #quit{}.

encode_response_values(#{}) ->
    [].

decode_response_values([<<>>]) ->
    #{}.
