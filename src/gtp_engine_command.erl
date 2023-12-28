-module(gtp_engine_command).
-include("gtp.hrl").

-callback command_name() -> CommandName :: binary().

-callback decode_command_arguments(EncodedArgs :: binary()) -> command().

-callback encode_response_values(ResponseValues :: response_values()) ->
    EncodedResponseValues :: [iodata()].
