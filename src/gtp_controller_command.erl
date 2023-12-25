-module(gtp_controller_command).
-include("gtp.hrl").

-callback encode_command(Command :: command()) -> EncodedCommand :: iodata().

-callback decode_response_values(EncodedResponseValues :: [binary()]) ->
    ResponseValues :: response_values().
