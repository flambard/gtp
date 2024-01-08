-module(gtp_command_undo).
-behaviour(gtp_controller_command).
-behaviour(gtp_engine_command).
-include("gtp.hrl").

-export([
    command_name/0,
    encode_command_arguments/1,
    decode_command_arguments/1
]).

% arguments: none
% effects: The board configuration and the number of captured stones are reset to the state
%   before the last move. The last move is removed from the move history.
% output: none
% fails: If the engine is unable to take back the last move, fails with the error message
%   "cannot undo".

command_name() ->
    <<"undo">>.

encode_command_arguments(#undo{}) ->
    [].

decode_command_arguments(<<>>) ->
    #undo{}.
