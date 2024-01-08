-module(gtp_command_final_score).
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
    <<"final_score">>.

encode_command_arguments(#final_score{}) ->
    [].

decode_command_arguments(<<>>) ->
    #final_score{}.

encode_response_values(#{score := Score}) ->
    [gtp_entity:encode(string, Score)].

decode_response_values([Line]) ->
    #{score => gtp_entity:decode_line(string, Line)}.
