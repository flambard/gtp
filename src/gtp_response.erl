-module(gtp_response).
-include("gtp.hrl").

-export([
    encode/3,
    decode/2
]).

%%%
%%% Encoding
%%%

encode(ID, #success{values = ResponseValues}, CommandMod) ->
    Values = [[<<" ">>, V] || V <- CommandMod:encode_response_values(ResponseValues)],
    [<<"=">>, encode_optional_id(ID), Values, <<"\n\n">>];
encode(ID, #failure{error_message = Error}, _CommandMod) ->
    [<<"?">>, encode_optional_id(ID), <<" ">>, Error, <<"\n\n">>].

encode_optional_id(undefined) -> <<>>;
encode_optional_id(ID) -> gtp_entity:encode_int(ID).

%%%
%%% Decoding
%%%

decode(_CommandMod, [<<"? ", Error/binary>>]) ->
    {undefined, #failure{error_message = Error}};
decode(_CommandMod, [<<"?", IdAndError/binary>>]) ->
    [BinID, Error] = binary:split(IdAndError, <<" ">>),
    {ID, []} = gtp_entity:decode_int(BinID),
    {ID, #failure{error_message = Error}};
decode(CommandMod, [<<"= ", ResponseValues/binary>> | Lines]) ->
    Response = #success{values = CommandMod:decode_response_values([ResponseValues | Lines])},
    {undefined, Response};
decode(CommandMod, [<<"=">>]) ->
    Response = #success{values = CommandMod:decode_response_values([<<>>])},
    {undefined, Response};
decode(CommandMod, [<<"=", IdAndResponseValues/binary>> | Lines]) ->
    [BinID, ResponseValues] = binary:split(IdAndResponseValues, <<" ">>),
    {ID, []} = gtp_entity:decode_int(BinID),
    Response = #success{values = CommandMod:decode_response_values([ResponseValues | Lines])},
    {ID, Response}.
