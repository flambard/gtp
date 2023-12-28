-module(gtp_controller_command).
-include("gtp.hrl").

-callback encode_command_arguments(Command :: command()) -> EncodedArguments :: [iodata()].

-callback decode_response_values(EncodedResponseLines :: [binary()]) ->
    ResponseValues :: response_values().
