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
    [gtp_entity:encode({list, string}, Name)].

decode_response_values([Line]) ->
    #{name => gtp_entity:decode_line({list, string}, Line)}.
