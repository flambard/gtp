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
encode_optional_id(ID) -> gtp_entity:encode_int(ID).

%%%
%%% Decoding
%%%

decode(_CommandMod, <<"? ", Error/binary>>) ->
    {undefined, #failure{error_message = Error}};
decode(_CommandMod, <<"?", IdAndError/binary>>) ->
    [BinID, Error] = binary:split(IdAndError, <<" ">>),
    {ID, []} = gtp_entity:decode_int(BinID),
    {ID, #failure{error_message = Error}};
decode(CommandMod, <<"= ", ResponseValues/binary>>) ->
    EncodedValues = binary:split(ResponseValues, <<" ">>, [global, trim_all]),
    Response = #success{values = CommandMod:decode_response_values(EncodedValues)},
    {undefined, Response};
decode(CommandMod, <<"=">>) ->
    Response = #success{values = CommandMod:decode_response_values([])},
    {undefined, Response};
decode(CommandMod, <<"=", IdAndResponseValues/binary>>) ->
    [BinID | EncodedValues] = binary:split(IdAndResponseValues, <<" ">>, [global, trim_all]),
    {ID, []} = gtp_entity:decode_int(BinID),
    Response = #success{values = CommandMod:decode_response_values(EncodedValues)},
    {ID, Response}.
