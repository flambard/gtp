-module(gtp_response).
-include("gtp.hrl").

-export([
    encode_success/2,
    encode_error/2,
    decode/2
]).

%%%
%%% Encoding
%%%

encode_success(ID, EncodedResponseValues) ->
    Values = [[<<" ">>, V] || V <- EncodedResponseValues],
    [<<"=">>, encode_optional_id(ID), Values].

encode_error(ID, Error) ->
    ["?", encode_optional_id(ID), <<" ">>, Error].

encode_optional_id(undefined) -> <<"">>;
encode_optional_id(ID) -> gtp_types:encode_int(ID).

%%%
%%% Decoding
%%%

decode(_CommandMod, <<"? ", Error/binary>>) ->
    {undefined, #failure{error_message = Error}};
decode(_CommandMod, <<"?", IdAndError/binary>>) ->
    [ID, Error] = binary:split(IdAndError, <<" ">>),
    {gtp_types:decode_int(ID), #failure{error_message = Error}};
decode(CommandMod, <<"= ", ResponseValues/binary>>) ->
    EncodedValues = binary:split(ResponseValues, <<" ">>, [global, trim_all]),
    {undefined, CommandMod:decode_response_values(EncodedValues)};
decode(CommandMod, <<"=">>) ->
    {undefined, CommandMod:decode_response_values([])};
decode(CommandMod, <<"=", IdAndResponseValues/binary>>) ->
    [ID | EncodedValues] = binary:split(IdAndResponseValues, <<" ">>, [global, trim_all]),
    {gtp_types:decode_int(ID), CommandMod:decode_response_values(EncodedValues)}.
