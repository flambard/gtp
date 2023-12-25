-module(gtp_command_known_command).
-behaviour(gtp_controller_command).
-behaviour(gtp_engine_command).
-include("gtp.hrl").

-export([
    decode_command_arguments/1,
    decode_response_values/1,
    encode_command/1,
    encode_response_values/1
]).

% arguments:
%   string command_name
% effects: none
% output:
%   boolean known
% fails: never

encode_command(#known_command{command_name = Name}) ->
    [<<"known_command">>, " ", Name].

decode_command_arguments(<<CommandName/binary>>) ->
    #known_command{command_name = CommandName}.

encode_response_values(#{known := Known}) ->
    [gtp_types:encode_boolean(Known)].

decode_response_values([Known]) ->
    #{known => gtp_types:decode_boolean(Known)}.
