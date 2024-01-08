-module(gtp_command_time_settings).
-behaviour(gtp_controller_command).
-behaviour(gtp_engine_command).
-include("gtp.hrl").

-export([
    command_name/0,
    encode_command_arguments/1,
    decode_command_arguments/1
]).

% arguments:
%   int main_time
%   int byo_yomi_time
%   int byo_yomi_stones
% effects: The time settings are changed
% output: none
% fails: syntax error

command_name() ->
    <<"time_settings">>.

encode_command_arguments(#time_settings{main_time = T, byo_yomi_time = BYT, byo_yomi_stones = BYS}) ->
    [gtp_entity:encode([int, int, int], [T, BYT, BYS])].

decode_command_arguments(Binary) ->
    [T, BYT, BYS] = gtp_entity:decode_line([int, int, int], Binary),
    #time_settings{main_time = T, byo_yomi_time = BYT, byo_yomi_stones = BYS}.
