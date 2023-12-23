-module(gtp_command_protocol_version).
-behaviour(gtp_command).
-include("gtp.hrl").

-export([
    encode_command/1,
    decode_command_arguments/1,
    encode_response_values/1,
    decode_response_values/1
]).

% arguments: none
% effects: none
% output:
%   int version_number
% fails: never

encode_command(#protocol_version{}) ->
    <<"protocol_version">>.

decode_command_arguments(<<>>) ->
    #protocol_version{}.

encode_response_values(#{version_number := Version}) ->
    [gtp_types:encode_int(Version)].

decode_response_values([EncodedVersion]) ->
    #success_response{values = #{version_number => gtp_types:decode_int(EncodedVersion)}}.
