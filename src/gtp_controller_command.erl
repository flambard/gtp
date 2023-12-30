-module(gtp_controller_command).
-include("gtp.hrl").

-callback command_name() -> CommandName :: binary().

-callback encode_command_arguments(Command :: command()) -> EncodedArguments :: [iodata()].

-callback decode_response_values(EncodedResponseLines :: [binary()]) ->
    ResponseValues :: response_values().

-optional_callbacks([
    decode_response_values/1
]).
