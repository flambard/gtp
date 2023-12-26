-module(gtp_types).

-include("gtp.hrl").

-export([
    encode_int/1,
    encode_float/1,
    encode_boolean/1,
    encode_color/1,
    encode_vertex/1,
    encode_move/1,
    encode_string/1,
    encode_list/2
]).

-export([
    decode_int/1,
    decode_float/1,
    decode_boolean/1,
    decode_color/1,
    decode_vertex/1,
    decode_move/1,
    decode_string/1,
    decode_list/2
]).

%%%
%%% Encoding
%%%

encode_int(Int) ->
    integer_to_binary(Int).

encode_float(Float) ->
    float_to_binary(Float).

encode_boolean(true) -> <<"true">>;
encode_boolean(false) -> <<"false">>.

encode_color(black) -> <<"black">>;
encode_color(white) -> <<"white">>.

encode_vertex({Letter, Number}) ->
    [atom_to_binary(Letter), integer_to_binary(Number)].

encode_move(#move{color = C, vertex = V}) ->
    [encode_color(C), " ", encode_vertex(V)].

encode_string(String) ->
    String.

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

decode_vertex(<<Letter:1/binary, Number/binary>>) ->
    L = binary_to_atom(Letter),
    {N, Rest} = decode_int(Number),
    {{L, N}, Rest}.

decode_move(Binary) ->
    {Color, [R1]} = decode_color(Binary),
    {Vertex, Rest} = decode_vertex(R1),
    {#move{color = Color, vertex = Vertex}, Rest}.

decode_string(Binary) ->
    {Binary, []}.

decode_list(DecodingFun, Binary) ->
    case DecodingFun(Binary) of
        {Value, []} -> [Value];
        {Value, [Rest]} -> [Value | decode_list(DecodingFun, Rest)]
    end.
