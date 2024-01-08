-module(gtp_command_final_status_list).
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

% arguments:
%   string status
% effects: none
% output:
%   vertex*& stones
% fails: never

command_name() ->
    <<"final_status_list">>.

encode_command_arguments(#final_status_list{status = Status}) ->
    [gtp_entity:encode(string, Status)].

decode_command_arguments(Binary) ->
    #final_status_list{status = gtp_entity:decode_line(string, Binary)}.

encode_response_values(#{stones := Stones}) ->
    [gtp_entity:encode({multiline, {list, vertex}}, Stones)].

decode_response_values(Lines) ->
    #{stones => gtp_entity:decode_multiline({list, vertex}, Lines)}.
