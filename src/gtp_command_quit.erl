-module(gtp_command_quit).
-behaviour(gtp_command).
-include("gtp.hrl").
-include("gtp_commands.hrl").

-export([
    encode_command/1,
    decode_command_arguments/1,
    encode_response_values/1,
    decode_response_values/1
]).

% arguments: none
% effects: The session is terminated and the connection closed.
% output: none
% fails: never

encode_command(#quit{}) ->
    <<"quit">>.

decode_command_arguments(<<>>) ->
    #quit{}.

encode_response_values(#{}) ->
    [].

decode_response_values([]) ->
    #success_response{}.
