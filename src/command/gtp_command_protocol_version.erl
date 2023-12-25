-module(gtp_command_protocol_version).
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
% effects: none
% output:
%   int version_number
% fails: never

name() ->
    <<"protocol_version">>.

encode_command_arguments(#protocol_version{}) ->
    [].

decode_command_arguments(<<>>) ->
    #protocol_version{}.

encode_response_values(#{version_number := Version}) ->
    [gtp_types:encode_int(Version)].

decode_response_values([EncodedVersion]) ->
    {Version, []} = gtp_types:decode_int(EncodedVersion),
    #{version_number => Version}.
