-module(gtp_decode_command_tests).
-include_lib("eunit/include/eunit.hrl").

decode_command_with_id_test() ->
    Message = <<"276 protocol_version">>,
    Result = gtp_command:decode(Message),
    #{
        id := 276,
        command := <<"protocol_version">>,
        module := gtp_command_protocol_version,
        arguments := <<>>
    } = Result.

decode_command_without_id_test() ->
    Message = <<"quit">>,
    Result = gtp_command:decode(Message),
    #{
        command := <<"quit">>,
        module := gtp_command_quit,
        arguments := <<>>
    } = Result.
