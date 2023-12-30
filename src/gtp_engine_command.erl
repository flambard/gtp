-module(gtp_engine_command).
-include("gtp.hrl").

-callback decode_command_arguments(EncodedArgs :: binary()) -> command().

-callback encode_response_values(ResponseValues :: response_values()) ->
    EncodedResponseValues :: [iodata()].

-optional_callbacks([
    encode_response_values/1
]).
