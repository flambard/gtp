-module(gtp_response).
-include("gtp.hrl").

-export([
    encode/3,
    decode/2
]).

%%%
%%% Encoding
%%%

encode(ID, #success{values = Values}, _CommandMod) when map_size(Values) == 0 ->
    [<<"=">>, encode_optional_id(ID), <<"\n\n">>];
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
    {ID, [Error]} = gtp_entity:decode_int(IdAndError),
    {ID, #failure{error_message = Error}};
decode(_CommandMod, [<<"=">>]) ->
    %% No response values to decode
    {undefined, #success{values = #{}}};
decode(_CommandMod, [<<"= ">>]) ->
    %% No response values to decode
    {undefined, #success{values = #{}}};
decode(CommandMod, [<<"= ", ResponseValues/binary>> | Lines]) ->
    Values = CommandMod:decode_response_values([ResponseValues | Lines]),
    {undefined, #success{values = Values}};
decode(CommandMod, [<<"=", IdAndResponseValues/binary>> | Lines]) ->
    {ID, [ResponseValues]} = gtp_entity:decode_int(IdAndResponseValues),
    Values =
        case [ResponseValues | Lines] of
            [<<>>] -> #{};
            NotEmpty -> CommandMod:decode_response_values(NotEmpty)
        end,
    {ID, #success{values = Values}}.
