-module(gtp_types).

-include("gtp.hrl").

-export([
    encode/1,
    encode_int/1,
    decode_int/1,
    encode_float/1,
    decode_float/1,
    encode_boolean/1,
    decode_boolean/1,
    encode_color/1,
    decode_color/1,
    decode_list/2,
    encode_list/2
]).

encode(Int) when is_integer(Int) ->
    integer_to_binary(Int);
encode(Float) when is_float(Float) ->
    float_to_binary(Float);
encode(true) ->
    <<"true">>;
encode(false) ->
    <<"false">>;
encode(white) ->
    <<"white">>;
encode(black) ->
    <<"black">>;
encode({Letter, Number}) ->
    %% Vertex
    [atom_to_binary(Letter), integer_to_binary(Number)];
encode(#move{color = C, vertex = V}) ->
    [encode(C), " ", encode(V)];
encode(Tuple) when is_tuple(Tuple) ->
    encode(tuple_to_list(Tuple));
encode(List) when is_list(List) ->
    lists:join(" ", [encode(E) || E <- List]);
encode(String) when is_binary(String) ->
    String.

%%%
%%% Encoding
%%%

encode_int(Integer) ->
    integer_to_binary(Integer).

encode_float(Float) ->
    float_to_binary(Float).

encode_boolean(true) -> <<"true">>;
encode_boolean(false) -> <<"false">>.

encode_color(black) -> <<"black">>;
encode_color(white) -> <<"white">>.

encode_list(EncodeFun, List) ->
    lists:join(<<" ">>, lists:map(EncodeFun, List)).

%%%
%%% Decoding
%%%

decode_int(Binary) ->
    [IntBin | Rest] = binary:split(Binary, <<" ">>, [trim]),
    {binary_to_integer(IntBin), Rest}.

decode_float(Binary) ->
    [FloatBin | Rest] = binary:split(Binary, <<" ">>, [trim]),
    {binary_to_float(FloatBin), Rest}.

decode_boolean(Binary) ->
    case binary:split(Binary, <<" ">>, [trim]) of
        [<<"true">> | Rest] -> {true, Rest};
        [<<"false">> | Rest] -> {false, Rest}
    end.

decode_color(Bin) ->
    case binary:split(Bin, <<" ">>, [trim]) of
        [<<"black">> | Rest] -> {black, Rest};
        [<<"white">> | Rest] -> {white, Rest}
    end.

decode_list(DecodingFun, Binary) ->
    case DecodingFun(Binary) of
        {Value, []} -> [Value];
        {Value, [Rest]} -> [Value | decode_list(DecodingFun, Rest)]
    end.
