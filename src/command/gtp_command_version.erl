-module(gtp_command_version).
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
%   string* version
% fails: never

command_name() ->
    <<"version">>.

encode_command_arguments(#version{}) ->
    [].

decode_command_arguments(<<>>) ->
    #version{}.

encode_response_values(#{version := Version}) ->
    [gtp_entity:encode({list, string}, Version)].

decode_response_values([Line]) ->
    #{version => gtp_entity:decode_line({list, string}, Line)}.
