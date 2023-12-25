-module(gtp_engine_command).
-include("gtp.hrl").

-callback decode_command_arguments(EncodedArgs :: binary()) -> command().

-callback encode_response_values(ResponseValues :: #{atom() => term()}) ->
    EncodedResponseValues :: [iodata()].
