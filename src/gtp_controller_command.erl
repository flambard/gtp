-module(gtp_controller_command).
-include("gtp.hrl").

%% TODO: command_name is actually required but generates a conflict warning with gtp_engine_command
%% -callback command_name() -> CommandName :: binary().

-callback encode_command_arguments(Command :: command()) -> EncodedArguments :: [iodata()].

-callback decode_response_values(EncodedResponseLines :: [binary()]) ->
    ResponseValues :: response_values().
