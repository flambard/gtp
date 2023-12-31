-module(gtp_command_list_commands).
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
%   string& commands
% fails: never

command_name() ->
    <<"list_commands">>.

encode_command_arguments(#list_commands{}) ->
    [].

decode_command_arguments(<<>>) ->
    #list_commands{}.

encode_response_values(#{commands := Commands}) ->
    [gtp_entity:encode({multiline, string}, Commands)].

decode_response_values(Lines) ->
    #{commands => gtp_entity:decode_multiline(string, Lines)}.
