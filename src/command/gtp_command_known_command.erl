-module(gtp_command_known_command).
-behaviour(gtp_controller_command).
-behaviour(gtp_engine_command).
-include("gtp.hrl").

-export([
    command_name/0,
    decode_command_arguments/1,
    decode_response_values/1,
    encode_command_arguments/1,
    encode_response_values/1
]).

% arguments:
%   string command_name
% effects: none
% output:
%   boolean known
% fails: never

command_name() -> <<"known_command">>.

encode_command_arguments(#known_command{command_name = Name}) ->
    [gtp_entity:encode(string, Name)].

decode_command_arguments(Binary) ->
    #known_command{command_name = gtp_entity:decode_line(string, Binary)}.

encode_response_values(#{known := Known}) ->
    [gtp_entity:encode(boolean, Known)].

decode_response_values([Line]) ->
    #{known => gtp_entity:decode_line(boolean, Line)}.
