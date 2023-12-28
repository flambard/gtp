-module(gtp_decode_command_tests).
-include_lib("eunit/include/eunit.hrl").
-include("gtp.hrl").

decode_command_with_id_test() ->
    Message = <<"276 protocol_version">>,
    Result = gtp_command:decode(Message),
    {276, #protocol_version{}, gtp_command_protocol_version} = Result.

decode_command_without_id_test() ->
    Message = <<"quit">>,
    Result = gtp_command:decode(Message),
    {undefined, #quit{}, gtp_command_quit} = Result.
