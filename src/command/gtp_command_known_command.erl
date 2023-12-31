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
    {Name, []} = gtp_entity:decode(string, Binary),
    #known_command{command_name = Name}.

encode_response_values(#{known := Known}) ->
    [gtp_entity:encode(boolean, Known)].

decode_response_values([Line]) ->
    {Known, []} = gtp_entity:decode(boolean, Line),
    #{known => Known}.
