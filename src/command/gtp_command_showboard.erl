-module(gtp_command_showboard).
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
%   string*& board
% fails: never

command_name() ->
    <<"showboard">>.

encode_command_arguments(#showboard{}) ->
    [].

decode_command_arguments(<<>>) ->
    #showboard{}.

encode_response_values(#{board := Board}) ->
    [gtp_entity:encode({multiline, {list, string}}, Board)].

decode_response_values(Lines) ->
    #{board => gtp_entity:decode_multiline({list, string}, Lines)}.
