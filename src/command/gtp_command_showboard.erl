-module(gtp_command_showboard).
-behaviour(gtp_controller_command).
-include("gtp.hrl").

-export([
    command_name/0,
    encode_command_arguments/1,
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

% decode_command_arguments(<<>>) ->
%     #showboard{}.

% encode_response_values(#{board := Board}) ->
%     not_implemented.

decode_response_values(Lines) ->
    Board = gtp_entity:decode_multiline({list, string}, Lines),
    #{board => Board}.
