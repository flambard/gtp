-module(gtp_command_protocol_version).
-behaviour(gtp_controller_command).
-behaviour(gtp_engine_command).
-include("gtp.hrl").

-export([
    command_name/0,
    encode_command_arguments/1,
    decode_command_arguments/1,
    encode_response_values/1,
    decode_response_values/1
]).

% arguments: none
% effects: none
% output:
%   int version_number
% fails: never

command_name() ->
    <<"protocol_version">>.

encode_command_arguments(#protocol_version{}) ->
    [].

decode_command_arguments(<<>>) ->
    #protocol_version{}.

encode_response_values(#{version_number := Version}) ->
    [gtp_entity:encode_int(Version)].

decode_response_values([Line]) ->
    {Version, []} = gtp_entity:decode_int(Line),
    #{version_number => Version}.
