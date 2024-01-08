-module(gtp_command_loadsgf).
-behaviour(gtp_controller_command).
-behaviour(gtp_engine_command).
-include("gtp.hrl").

-export([
    command_name/0,
    encode_command_arguments/1,
    decode_command_arguments/1
]).

% arguments:
%   string filename
%   int move_number (optional)
% effects: The SGF file is loaded configures the board accordingly.
% output: none
% fails: Syntax error. If the file does not exist or cannot be read it fails with the error
%   message "cannot load file"

command_name() ->
    <<"loadsgf">>.

encode_command_arguments(#loadsgf{filename = Filename, move_number = Move}) ->
    [gtp_entity:encode([string, int], [Filename, Move])].

decode_command_arguments(Binary) ->
    [Filename, Move] = gtp_entity:decode_line([string, int], Binary),
    #loadsgf{filename = Filename, move_number = Move}.
