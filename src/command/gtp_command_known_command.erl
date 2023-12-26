-module(gtp_command_known_command).
-behaviour(gtp_controller_command).
-behaviour(gtp_engine_command).
-include("gtp.hrl").

-export([
    name/0,
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

name() -> <<"known_command">>.

encode_command_arguments(#known_command{command_name = Name}) ->
    [Name].

decode_command_arguments(<<CommandName/binary>>) ->
    #known_command{command_name = CommandName}.

encode_response_values(#{known := Known}) ->
    [gtp_entity:encode_boolean(Known)].

decode_response_values([Known]) ->
    {IsKnown, []} = gtp_entity:decode_boolean(Known),
    #{known => IsKnown}.
