-module(gtp_command_komi).
-behaviour(gtp_controller_command).
-behaviour(gtp_engine_command).
-include("gtp.hrl").

-export([
    command_name/0,
    encode_command_arguments/1,
    decode_command_arguments/1
]).

% arguments:
%   float new_komi
% effects: Komi is changed
% output: none
% fails: syntax error

command_name() ->
    <<"komi">>.

encode_command_arguments(#komi{new_komi = Komi}) ->
    [gtp_entity:encode(float, Komi)].

decode_command_arguments(Binary) ->
    #komi{new_komi = gtp_entity:decode_line(float, Binary)}.
