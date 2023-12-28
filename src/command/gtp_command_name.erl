-module(gtp_command_name).
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
%   string* name
% fails: never

command_name() ->
    <<"name">>.

encode_command_arguments(#name{}) ->
    [].

decode_command_arguments(<<>>) ->
    #name{}.

encode_response_values(#{name := Name}) ->
    [gtp_entity:encode_list(fun gtp_entity:encode_string/1, Name)].

decode_response_values([Name]) ->
    {NameStrings, []} = gtp_entity:decode_list(fun gtp_entity:decode_string/1, Name),
    #{name => NameStrings}.
